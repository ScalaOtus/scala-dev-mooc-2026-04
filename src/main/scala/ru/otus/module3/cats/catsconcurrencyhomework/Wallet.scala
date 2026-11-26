package ru.otus.module3.cats.catsconcurrencyhomework


import cats.effect.{Resource, Sync}
import cats.implicits.*
import Wallet.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// реализовать используя java.nio.file._
// Насчёт безопасного конкурентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// Функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get

// Кошелёк FileWallet является stateless: сам объект FileWallet не хранит ни поля с балансом, ни кэша.
// Единственный источник истины — файл на диске, и каждая операция честно ходит к нему за свежими данными.
// Минусы: возможна гонка при конкурентном доступе к файлу; относительно медленные операции из-за файлового I/O.
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  private val file: Path = FileWallet.pathFor(id)

  // метод write для хранения в файле (только для внутреннего использования)
  private def write(value: BigDecimal): F[Unit] =
    Sync[F].blocking(
      Files.write(file, value.toString.getBytes(StandardCharsets.UTF_8))
    )

  // приватный метод для чтения баланса из файла
  private def read: F[BigDecimal] =
    Sync[F].blocking(Files.readString(file)).map(s => BigDecimal(s.trim))

  // возвращает текущий баланс
  override def balance: F[BigDecimal] = read

  // пополняет баланс на указанную сумму
  override def topup(amount: BigDecimal): F[Unit] =
    read.flatMap(current => write(current + amount))
    // for
    //   current <- read
    //   _ <- write(current + amount)
    // yield ()

  // списывает указанную сумму с баланса (ошибка, если средств недостаточно)
  override def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
    read.flatMap { current =>
      if (current < amount)
        Sync[F].pure(Left(BalanceTooLow): Either[WalletError, Unit])
      else
        write(current - amount).map(_ => Right(()))
    }
}

object FileWallet {
  def pathFor(id: WalletId): Path =
    Paths.get(System.getProperty("java.io.tmpdir"), s"$id.balance")
}

object Wallet {
  // Смарт-конструктор - фабрика для создания готового экземпляра Wallet.
  // Создаёт файл с нулевым балансом (если нужно) и возвращает готовый интерпретатор (работающий со своим файлом).
/*  def fileWallet[F[_] : Sync](id: WalletId): F[Wallet[F]] =
    Sync[F].blocking {
      val initialPath = FileWallet.pathFor(id)
      if (!Files.exists(initialPath))   // Файл с начальным балансом 0 создаётся ровно один раз: когда файла нет
        Files.write(initialPath, BigDecimal(0).toString.getBytes(StandardCharsets.UTF_8))
      new FileWallet[F](id)
    }*/

  def fileWallet[F[_] : Sync](id: WalletId): Resource[F, Wallet[F]] =
    Resource.make(
      Sync[F].blocking {
        val initialPath = FileWallet.pathFor(id)
        if (!Files.exists(initialPath))
          Files.write(initialPath, BigDecimal(0).toString.getBytes(StandardCharsets.UTF_8))
        new FileWallet[F](id)
      }
    )(_ => Sync[F].blocking(Files.deleteIfExists(FileWallet.pathFor(id))).void)

  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}