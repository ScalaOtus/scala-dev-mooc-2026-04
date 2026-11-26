package ru.otus.module3.cats.catsconcurrencyhomework

import cats.effect.{IO, IOApp, Resource}
import cats.implicits.*

import scala.concurrent.duration.{DurationInt, Duration}

// Поиграемся с кошельками на файлах и файберами.

// 1. Описать бесконечный процесс регулярного пополнения кошелька на 100р каждые N миллисекунд.
// 2. Запустить 3 фоновых файбера, в каждом должен быть запущен описанный выше процесс (у каждого файбера свой
//    отдельный кошелек); промежуток надо сделать разный, чтобы легче было наблюдать разницу (100ms, 500ms, 2000ms).
// 3. Описать бесконечный процесс регулярного вывода: раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// 4. Запустить процесс из п.3 как четвертый фоновый файбер.
// 5. Добавить в основной файбер логику ожидания ввода пользователя (IO.readline) и завершения работы программы
//    (включая все фоновые процессы), когда ввод будет получен.

// Итого у нас 5 процессов:
//  3 фоновых процесса регулярного пополнения кошельков,
//  1 фоновый процесс регулярного вывода балансов на экран и
//  1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())

object WalletFibersApp extends IOApp.Simple {

  // 1) бесконечный процесс регулярного пополнения кошелька на 100р каждые N миллисекунд
  def topUpLoop(wallet: Wallet[IO], interval: Duration): IO[Unit] = {
    def loop(): IO[Unit] =
      wallet.topup(100)
        .flatMap(_ => IO.sleep(interval))
        .flatMap(_ => loop())
    
    loop()
  }

  // 3) Описание процесса регулярного вывода в консоль балансов всех трёх кошельков
  def printLoop(w1: Wallet[IO], w2: Wallet[IO], w3: Wallet[IO]): IO[Unit] = {
    def loop(): IO[Unit] =
      for {
        b1 <- w1.balance
        b2 <- w2.balance
        b3 <- w3.balance
        _  <- IO.println(s"wallet balances 1: $b1 | 2: $b2 | 3: $b3")
        _  <- IO.sleep(1.second)
        _  <- loop()
      } yield ()

    loop()
  }

  // Сброс кошелька в начальное состояние (0)
  /*private def reset(wallet: Wallet[IO]): IO[Unit] =
    wallet.balance.flatMap { b =>
      if (b == BigDecimal(0)) IO.unit
      else wallet.withdraw(b).void
    }*/

  def run: IO[Unit] =
    (for {
      _ <- Resource.eval(IO.println("Press Enter to stop..."))

      // кошельки приходят как Resource[IO, Wallet[IO]] (поэтому не нужен Resource.eval)
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")

      _ <- Resource.eval(IO.println(s"Wallet 1 path: ${FileWallet.pathFor("1")}"))
      _ <- Resource.eval(IO.println(s"Wallet 2 path: ${FileWallet.pathFor("2")}"))
      _ <- Resource.eval(IO.println(s"Wallet 3 path: ${FileWallet.pathFor("3")}"))

      // сброс балансов в ноль перед стартом файберов (уже сброшены из прошлого Resource)
      // _ <- Resource.eval(List(wallet1, wallet2, wallet3).traverse_(reset))

      // фоновые файберы пополнения (старт/отмена файбера автоматически как часть .background)
      _ <- topUpLoop(wallet1, 100.millis).background
      _ <- topUpLoop(wallet2, 500.millis).background
      _ <- topUpLoop(wallet3, 2000.millis).background

      // фоновый файбер с выводом балансов
      _ <- printLoop(wallet1, wallet2, wallet3).background

      // ждём Enter — после выхода из use файберы отменятся как Resource через background,
      // при закрытии/отмене для каждого кошелька release удалит свой файл
      _ <- Resource.eval(IO.readLine)
    } yield ()
    ).use(
      _ => IO.println("Stopped.")
    )
}