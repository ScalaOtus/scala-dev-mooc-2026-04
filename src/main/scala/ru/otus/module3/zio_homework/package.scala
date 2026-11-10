package ru.otus.module3

import zio.{Console, IO, Random, Task, UIO, ZIO}

import java.io.IOException
import scala.language.postfixOps
import zio_homework.config.Configuration

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу, которая будет предлагать пользователю
   * угадать число от 1 до 3 и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее
   * можно декомпозировать.
   */
/*
  1. Загадать число от 1 до 3 — эффект случайности (Random.nextIntBetween(1, 3)).
  2. Напечатать приглашение — чистый консольный эффект.
  3. Прочитать строку и превратить её в число — эффект чтения (Console.readLine) плюс чистый парсинг, который может упасть, поэтому оборачиваем в ZIO.attempt.
  4. Напечатать результат угадывания — чистое сравнение (==), обёрнутое в эффект печати.
*/

  // Эффект для ввода строки и конвертирования её в Int.
  // Возможные исключения: IOException, NumberFormatException (это подтипы Throwable).
  // Итоговый тип: IO[Throwable, Int], он же Task[Int]
  val inputGuess: Task[Int] = {
    Console.readLine.flatMap { line =>
      ZIO.attempt(line.trim.toInt)
    }
  }

  // Чистая логика и эффект печати результата.
  def report(guess: Int, secretNumber: Int): IO[IOException, Unit] = {
    if (guess == secretNumber)
      Console.printLine(s"Угадал!")
    else
      Console.printLine(s"Не угадал: я загадал число $secretNumber")
  }

  // объединив Nothing (от secret), IOException (от консоли) и Throwable (от парсинга).
  lazy val guessProgram: Task[Boolean] =
    for {
      hidden <- Random.nextIntBetween(1, 3)
      _ <- Console.printLine("Попробуйте угадать число от 1 до 3.\nВедите число:")
      guess <- inputGuess
      _ <- report(guess, hidden)
      result = (guess == hidden)   // можно, потому что это чистое вычисление, которое из for разворачивается через .map()
    } yield (result)

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор,
   *    пока его значение в условии не даст true
   * 
   */
  /**
   Из условий задачи следует:
   1) На входе даны
      - эффект в общем виде: effect: ZIO[R, E, A]
      - условие: cond: A => Boolean
   2) doWhile повторяет effect, пока cond(a) возвращает false. Останавливается при первом результате cond(a) == true.
   3) Значение результата не требуется, поэтому на выходе ZIO[R, E, Unit].
   Получается сигнатура
      def doWhile[R, E, A](effect: ZIO[R, E, A])(cond: A => Boolean): ZIO[R, E, Unit]
   Делаю функцию каррированной для удобства использования (сначала эффект, потом условие).
   Поскольку guessProgram (задача 1) у меня возвращает эффект с результатом Task[Boolean], то удобно его использовать
   для запуска doWhile:
      doWhile(guessProgram)(result => result)
   Тогда игра guessProgram будет повторяться до выигрыша-угадывания (result == true) (или прекратиться при ошибке).
  */
  def doWhile[R, E, A](effect: ZIO[R, E, A])(cond: A => Boolean): ZIO[R, E, Unit] = {
    effect.flatMap { a =>
      if (cond(a)) ZIO.succeed(())
      else doWhile(effect)(cond)
    }
  }
  /* альтернатива (Scala 3)
  for
    a <- effect
    _ <- if cond(a)
      then ZIO.unit
      else doWhile(effect)(cond)
  yield ()
  */

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет
   *    дефолтный конфиг и выведет его в консоль.
   * Используйте эффект "Configuration.config" из пакета config
   */
  def loadConfigOrDefault: UIO[config.AppConfig] =
    Configuration.config.catchAll { error =>
      Console
        .printLine(s"Ошибка чтения конфига:\n$error\n")
        .ignore *>    // UIO[Unit]
        Console
          .printLine(s"Использую дефолтный конфиг: host = ${Configuration.defaultConfig.host}, port = ${Configuration.defaultConfig.port}\n")
          .ignore *>
          ZIO.succeed(Configuration.defaultConfig)
    }
  // запуск с ошибкой и её обработкой
  // sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"

  // запуск без ошибки
  // HOST=localhost PORT=8080 sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = ???

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = ???

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = ???


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = ???


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ???

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = ???

}
