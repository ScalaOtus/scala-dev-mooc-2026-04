package ru.otus.module3.cats.catsconcurrencyhomework

import cats.effect.{IO, IOApp}
import cats.implicits._

object WalletMain extends IOApp.Simple {

/*  def run: IO[Unit] =
    val walletName = "test_wallet"
    for {
      wallet <- Wallet.fileWallet[IO](walletName)
//      _ <- IO.println(s"Wallet path: ${FileWallet.pathFor(walletName)}")  // для отладки
      _ <- wallet.topup(100.0)
      _ <- wallet.balance.flatMap(IO.println)
      _ <- wallet.withdraw(50.0)
      _ <- wallet.balance.flatMap(IO.println)
//       bad <- wallet.withdraw(500.0)
//       _   <- IO.println(s"withdraw: $bad")      // Left(BalanceTooLow)
    } yield ()
*/
  // версия 2 (на Resource, с удалением файла кошелька)
  def run: IO[Unit] =
    Wallet.fileWallet[IO]("test_wallet").use { wallet =>
      for {
        _ <- wallet.topup(100.0)
        _ <- wallet.balance.flatMap(IO.println)
        _ <- wallet.withdraw(50.0)
        _ <- wallet.balance.flatMap(IO.println)
      } yield ()
    }
}