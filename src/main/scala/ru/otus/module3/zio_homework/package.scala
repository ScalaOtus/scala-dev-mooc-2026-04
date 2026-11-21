//noinspection ScalaWeakerAccess
package ru.otus.module3

//import ru.otus.module3.BuildingZIOServices.validateEnv
//import ru.otus.module3.ResourceApp.validateEnv
//import zioConcurrency.printEffectRunningTime
import ru.otus.module3.BuildingZIOServices.validateEnv
import zio.{Clock, Console, IO, Random, Task, UIO, ULayer, ZIO, ZLayer, durationInt}

import java.io.IOException
import scala.language.postfixOps
import zio_homework.config.Configuration

import java.util.concurrent.TimeUnit

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу, которая будет предлагать пользователю
   * угадать число от 1 до 3 и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее
   * можно декомпозировать.
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

  lazy val guessProgram: Task[Boolean] =
    for {
      hidden <- Random.nextIntBetween(1, 3)
      _ <- Console.printLine("Попробуйте угадать число от 1 до 3.\nВедите число:")
      guess <- inputGuess
      _ <- report(guess, hidden)
      result = (guess == hidden)   // чистое вычисление, разворачивается из for как .map()
    } yield result

  
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
  /* альтернатива (Scala 3 и for)
  for {
    a <- effect
    _ <- if cond(a)
      then ZIO.unit
      else doWhile(effect)(cond)
  } yield ()
  */

  
  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения,
   *    а в случае ошибки вернет дефолтный конфиг и выведет его в консоль.
   *    Используйте эффект "Configuration.config" из пакета config.
   */
  
  def loadConfigOrDefault: UIO[config.AppConfig] =
    Configuration.config.foldZIO(
      failure = error => {
        val default = Configuration.defaultConfig
        Console
          .printLine(s"Ошибка чтения конфига из переменных окружения:\n$error\n")
          .ignore *>    // UIO[Unit]
          Console
            .printLine(s"Подключаюсь с параметрами по умолчанию: host = ${default.host}, port = ${default.port}\n")
            .ignore *>
          ZIO.succeed(default)
      },
      success = cfg =>
        Console
          .printLine(s"Подключаюсь по заданными параметрам к ${cfg.host}:${cfg.port}")
          .ignore *>
          ZIO.succeed(cfg)
    )
    

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */

  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: UIO[Int] =
    for {
      _ <- ZIO.sleep(1 seconds)   // UIO[Int]
      random <- Random.nextIntBetween(0, 10)  // UIO[Int]
    } yield random

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[UIO[Int]] = (0 until 10).map(_ => eff).toList

  /**
   * 4.3 Напишите программу, которая
   *  - вычислит сумму элементов коллекции "effects",
   *  - напечатает ее в консоль и вернет результат,
   *  - а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  val sumSequential: UIO[Int] =
    ZIO.collectAll(effects)   // описание запуска всех 10 эффектов последовательно, List[UIO[Int]] => UIO[List[Int]]
      .map(_.sum)

// Когда эффект sumSequential будет исполняться рантаймом (в run), один файбер будет последовательно запускать эффекты
// из `effects`, и результаты будут собраны в List[Int], и для этого списка будет посчитана _.sum

  lazy val app: ZIO[Any, IOException, Int] = for {
    _ <- Console.printLine("Эффекты выполняются последовательно...")
    s <- zioConcurrency.printEffectRunningTime(sumSequential)
    _ <- Console.printLine(s"Сумма: $s")
  } yield s


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val sumParallel: UIO[Int] =
    ZIO.collectAllPar(effects) // описание запуска всех 10 эффектов параллельно, List[UIO[Int]] => UIO[List[Int]]
      .map(_.sum)

  // Когда рантайм (в run) будет исполнять эффект sumParallel, то он запустит отдельный файбер для каждого
  // эффекта из effects (10 файберов будут исполняться параллельно) - это минимизирует время выполнения.
  // Результаты так же будут собраны в List[Int] и будет посчитана _.sum

  lazy val appSpeedUp: ZIO[Any, IOException, Int] = for {
    _ <- Console.printLine("Эффекты выполняются параллельно...")
    s <- zioConcurrency.printEffectRunningTime(sumParallel)
    _ <- Console.printLine(s"Сумма: $s")
  } yield s


  /**
   * 5. Оформите ф-цию printEffectRunningTime, разработанную на занятиях, в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine, например
   */
  
  trait TimingService {
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
  }

  object TimingService {
    val live: ULayer[TimingService] =
      ZLayer.succeed[TimingService](new TimingService {
        override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
          for {
            start <- Clock.currentTime(TimeUnit.MILLISECONDS)
            r <- zio
            end <- Clock.currentTime(TimeUnit.MILLISECONDS)
            _ <- Console.printLine(s"Running time: ${end - start} ms").catchAllCause(_ => ZIO.unit)
          } yield r
      })

    // Акцессор: достаёт реализацию из окружения и дергает её метод
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R & TimingService, E, A] =
      ZIO.serviceWithZIO[TimingService](_.printEffectRunningTime(zio))
  }

  lazy val app5: ZIO[TimingService, IOException, Unit] =
    for {
      _ <- Console.printLine("Последовательное исполнение...")
      _ <- TimingService.printEffectRunningTime(sumSequential.unit) // ~10 секунд
      _ <- Console.printLine("\nПараллельное исполнение...")
      _ <- TimingService.printEffectRunningTime(sumParallel.unit)    // ~1 секунда
    } yield ()


  /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[TimingService, IOException, Unit] =
    for {
      _ <- Console.printLine("Задача 6. Последовательное исполнение...")
      _ <- TimingService.printEffectRunningTime(sumSequential.unit)
    } yield ()
  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Any, IOException, Unit] =
    appWithTimeLogg.provide(TimingService.live)

}
