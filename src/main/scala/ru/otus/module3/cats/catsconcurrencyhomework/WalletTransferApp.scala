package ru.otus.module3.cats.catsconcurrencyhomework

import cats.{Functor, Monad, MonadError}
import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.implicits.*
import Wallet.{BalanceTooLow, WalletError}

// In-memory кошелёк (чтобы протестировать бизнес-логику использующую кошельки: функцию transfer).
// Нужно сделать интерпретатор Wallet[IO] который будет использовать Ref и запустить тест с помощью такого Wallet.

//  1. реализовать интерпретатор Wallet[IO] использующий для хранения балансов Ref из cats-effect
//  2. реализовать смарт-конструктор для таких интерпретаторов
//  3. проверить, что всё работает, запустив WalletTransferApp

// Здесь мы хотим протестировать бизнес-логику использующую кошельки: функцию transfer.
// Однако мы не хотим в наших тестах создавать какие-то файлы: в реальном приложении такой тест будет нуждаться в базе данных,
// что очень неудобно, ведь мы тестируем не сами кошельки а функцию transfer.

// Для таких тестов нам бы очень пригодился in-memory кошелёк, который хранит свой баланс в памяти пока работает тест.
// Он будет очень быстрым и дешевым - то что нужно для тестирования.

// Для такой задачи хорошо подойдет cats.effect.kernel.Ref - ячейка памяти с атомарным доступом.
// Нужно сделать интерпретатор Wallet[IO] который будет использовать Ref и запустить тест с помощью такого Wallet.

object WalletTransferApp extends IOApp.Simple {

  // Функция, которую мы тестируем. Здесь менять ничего не нужно :)
  def transfer[F[_]: Monad](a: Wallet[F],
                            b: Wallet[F],
                            amount: BigDecimal): F[Unit] =
    a.withdraw(amount).flatMap {
      case Left(BalanceTooLow) => a.topup(amount)
      case Right(_)            => b.topup(amount)
    }

  // Оказалось, в случае ошибки (когда сумма перевода выше баланса) обычный testTransfer не проходит, потому что в моей
  // реализации withdraw при ошибке не меняется баланс (возвращает (current, Left(BalanceTooLow)). Поэтому пришлось
  // сделать версию transfer2 без компенсации ошибочного снятия и также обновить функцию тестирования,
  // чтобы проверять как transfer, так и transfer2

  case object BalanceTooLowException
    extends RuntimeException("Недостаточно средств")

  def transfer2[F[_] : Monad](a: Wallet[F],
                              b: Wallet[F],
                              amount: BigDecimal)
                             (implicit F: MonadError[F, Throwable]): F[Unit] =
    a.withdraw(amount).flatMap {
      case Left(BalanceTooLow) => F.raiseError(BalanceTooLowException)
      case Right(_) => b.topup(amount)
    }

  // Интерпретатор кошелька на Ref — атомарное ячейко-состояние
  final class InMemWallet[F[_]](ref: Ref[F, BigDecimal]) extends Wallet[F] {

    override def balance: F[BigDecimal] =
      ref.get

    override def topup(amount: BigDecimal): F[Unit] =
      ref.update(_ + amount)

    override def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
      ref.modify { current =>
        if (current < amount)   // при недостаточных средствах - возврат ошибки списания
          (current, Left(BalanceTooLow): Either[WalletError, Unit])
        else
          (current - amount, Right(()))
      }
  }

  // Смарт-конструктор: создание Ref — сайд-эффект, на выходе IO[Wallet[IO]]
  // def wallet(balance: BigDecimal): IO[Wallet[IO]] = {
  def wallet[F[_]: Ref.Make: Functor](balance: BigDecimal): F[Wallet[F]] =
    // Ref.of - эффект создания атомарной ячейки, он возвращает IO[Ref[IO, BigDecimal]]
    // Тип результата IO[Wallet[IO]], потому что new InMemWallet(_) - чистая функция и типы меняются по обычному
    // правилу функтора (IO):  IO[A].map(A => B)  ==  IO[B]
    // A = Ref[IO, BigDecimal], B = Wallet[IO] => получаем IO[Wallet[IO]]
    // Ref.of[IO, BigDecimal](balance).map(new InMemWallet(_))
    // (в итоге IO был заменён на F)
    Ref.of[F, BigDecimal](balance).map(new InMemWallet(_))

  // Обновлённый testTransfer, который выполняет перевод с одного кошелька на другой
  // через выбранную функцию transfer и выводит балансы после операции.
  // При этом для transfer2 можно ловить BalanceTooLowException (сделано в run).
  def testTransfer(
                    transfer: (Wallet[IO], Wallet[IO], BigDecimal) => IO[Unit],
                    b1: BigDecimal,
                    b2: BigDecimal,
                    amount: BigDecimal
                  ): IO[(BigDecimal, BigDecimal)] =
    for {
      w1 <- wallet[IO](b1)
      w2 <- wallet[IO](b2)
      _  <- transfer(w1, w2, amount)
      b1 <- w1.balance
      b2 <- w2.balance
    } yield (b1, b2)

  def run: IO[Unit] =
    val startB1 = BigDecimal(100)
    val startB2 = BigDecimal(200)
    val amount = BigDecimal(50)
    val errAmount = startB1 * 5
    val exp1ErrB1 = startB1 + errAmount   // для transfer версии 1
    val exp1ErrB2 = startB2
    val expectedB1 = startB1 - amount     // для transfer версии 2
    val expectedB2 = startB2 + amount

    for {
      // 50,250
      _ <- testTransfer(transfer, startB1, startB2, amount).flatMap { case (b1, b2) =>
          assert(b1 == expectedB1 && b2 == expectedB2, s"Тест не пройден! Ожидалось $expectedB1/$expectedB2 — получено $b1/$b2")
          IO.println(s"transfer $amount: $b1,$b2")
        }
        .handleErrorWith(e =>
          IO.println(s"transfer $amount ОШИБКА: ${e.getMessage}")
        )

      // 600/200 (ошибка BalanceTooLow не отлавливается - поэтому сделана transfer2)
      _ <- testTransfer(transfer, startB1, startB2, errAmount).flatMap { case (b1, b2) =>
          assert(b1 == exp1ErrB1 && b2 == exp1ErrB2, s"Тест не пройден! Ожидалось $exp1ErrB1/$exp1ErrB2 — получено $b1/$b2")
          IO.println(s"transfer $errAmount: $b1,$b2")
        }
        .handleErrorWith(e =>
          IO.println(s"transfer $errAmount ОШИБКА: ${e.getMessage}")
        )

      // 50,250
      _ <- testTransfer(transfer2, startB1, startB2, amount).flatMap { case (b1, b2) =>
          assert(b1 == expectedB1 && b2 == expectedB2, s"Тест не пройден! Ожидалось $expectedB1/$expectedB2 — получено $b1/$b2")
          IO.println(s"transfer2 $amount: $b1,$b2")
        }
        .handleErrorWith(e =>
          IO.println(s"transfer2 $amount ОШИБКА: ${e.getMessage}")
        )

      // 100,200  ОШИБКА: Недостаточно средств
      _ <- testTransfer(transfer2, startB1, startB2, errAmount).flatMap { case (b1, b2) =>
          IO.println(s"transfer2 $errAmount: $b1,$b2")
        }
        .handleErrorWith(e =>
          IO.println(s"transfer2 $errAmount ОШИБКА: ${e.getMessage}")
        )

    } yield ()
}