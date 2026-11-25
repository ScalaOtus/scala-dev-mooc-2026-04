package ru.otus.module1

import scala.util.Random

object BallsExperiment {

  def main(args: Array[String]): Unit = {
    println(s"Эксперимент:\n  Дана урна с шарами: ${BallBox.fixedBlackWhiteBox}.")
    println("  Какова условная вероятность вытащить из урны 1-й шар чёрный (0) и 2-й шар белый (1)?\n")
    // Фиксируем seed для воспроизводимости (важно для отладки и тестов)
    given r: Random = new Random(12345)

    val experimentsCount = 10_000

    // Создаём коллекцию из 10 000 одинаковых начальных коробок
    // List.fill вызывает fixedBlackWhiteBox один раз и дублирует ссылку,
    // получится 10_000 стартовых состояний - ok, тк. каждый вызов map построит свою цепочку новых объектов (по смыслу как новый эксперимент)
    val boxes: List[BallBox] = List.fill(experimentsCount)(BallBox.fixedBlackWhiteBox)

    // Проводим эксперимент (симуляция выполнение AB) на "каждой коробке"
    // (событие AB - в первом испытании появится черный шар, а во втором — белый),
    // Другими словами делаем новый эксперимент на одной и той же коробке, и считаем количество успешных.
    val successCountAB: Int = boxes
      .map(_.pullTwoBlackThenWhite(using r))
      .count(identity)
    // Вычисляем эмпирическую вероятность P(AB)
    val probabilityAB: Double = successCountAB.toDouble / experimentsCount

    // То же самое для события A (1-й шар чёрный), считаем P(A)
    val successCountA: Int = boxes
      .map(_.pullFirstIsBlack(using r))
      .count(identity)
    val probabilityA: Double = successCountA.toDouble / experimentsCount

    // условная вероятность P(A|B) = P(AB) / P(A)
    val conditionalProbabilityAB = probabilityAB / probabilityA
    println(s"Проведено экспериментов: $experimentsCount")
    println(s"Успешных (1-й чёрный): $successCountA")
    println(s"Успешных (1-й чёрный, 2-й белый): $successCountAB")
    println(f"Эмпирическая вероятность A: $probabilityA%.3f")
    println(f"Эмпирическая вероятность AB: $probabilityAB%.3f")
    println(f"Условная вероятность P(A|B) = P(AB) / P(A): $conditionalProbabilityAB%.3f")
    println(f"Теоретическая вероятность P(A|B): 3 / 5")
  }
}
