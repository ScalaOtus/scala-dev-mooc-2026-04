package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.task

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правовой результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  private type FutureFSResult[A] = Future[(List[A], List[Throwable])]

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): FutureFSResult[A] = {
    // Буду работать со списком в функциональном стиле, т.е. декларативно его описывать-строить, используя рекурсию
    // pattern matching и разделение на голову и хвост (как в LinkedList)
    futures match {
      // Если список фьюч пуст, то успехов и ошибок нет:
      case Nil => Future.successful((Nil, Nil))

      // Для непустого списка разделим список на голову и хвост:
      //   head: один текущий Future[A]
      //   tail: список остальных Future[A]
      case head :: tail =>
      // рекурсивно посчитаем результат для tail (fullSequence(tail) типа FutureFSResult[A], а потом сделаем его
      // композицию с head (типа Future[...]) через flatMap, что вернёт новую Future[...],
      // внутри которой будет обновлённый список
        fullSequence(tail)  // получение результата для хвоста - FutureFSResult[A]
          .flatMap { case (tailSuccesses, tailFailures) => // здесь есть: композиция Future (tail и head) через flatMap,
              // отложенное выполнение (пока не будет готова Future tail)
              // и после этого "распаковка" через pm (tailSuccesses уже List)
            head.map { value =>  // когда Future head становится успешна, тогда есть результат A (value) из head,
              (value :: tailSuccesses, tailFailures)  // и тогда надо его добавить в список результатов
            }.recover {  // если Future head упала с исключением ex, то ловим его и добавляем в список ошибок
              case ex: Throwable =>
                (tailSuccesses, ex :: tailFailures)
            }
        }
    }
  }
}
