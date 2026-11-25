package ru.otus.module1

import scala.util.Random


final case class BallBox(balls: List[Int]) {
  /**
   * Вытащить один шарик без возврата.
   * Возвращает кортеж (новая коробка, Some(true) если чёрный (0), Some(false) если белый (1)).
   * Если коробка пуста — (this, None).
   */
  def pullOne(using r: Random): (BallBox, Option[Boolean]) =
    if balls.isEmpty then (this, None)
    else
      val idx = r.nextInt(balls.size)
      val (before, afterWithHead) = balls.splitAt(idx)
      val ball :: after = afterWithHead // safe: isEmpty уже проверен
      val isBlack = ball == 0   // 0 = чёрный, 1 = белый
      (BallBox(before ++ after), Some(isBlack))

  def pullFirstIsBlack(using r: Random): Boolean =
    pullOne(using r) match
      case (_, Some(isBlack)) => isBlack
      case (_, None) => false

  /**
   * Вытащить два шарика без возврата: 1-й чёрный и 2-й белый (это событие AB)
   * Возвращает true, если условие задачи (1 чёрный, 2 белый) выполнено, иначе false.
   */
  def pullTwoBlackThenWhite(using r: Random): Boolean =
    val (boxAfter1, firstOpt) = this.pullOne(using r)
    firstOpt match
      case None => false
      case Some(firstIsBlack) if !firstIsBlack => false // первый не чёрный
      case Some(_) =>
        val (_, secondOpt) = boxAfter1.pullOne(using r)
        secondOpt match
          case None => false
          case Some(secondIsBlack) => !secondIsBlack // второй должен быть белым
}

object BallBox {
  def fixedBlackWhiteBox: BallBox = BallBox(List(0, 0, 0, 1, 1, 1))
}
