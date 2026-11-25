package ru.otus.module1.collectionsHW

object collectionsTask {
  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum","dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    val transform: PartialFunction[(String, Int), String] = {
      case (value, 0) => value
      case (value, _) if isASCIIString(value) => value.toUpperCase
      case (value, _) => value.toLowerCase
    }
    text.zipWithIndex.collect(transform)
  }

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val textNumbers = Map(
      "0" -> "zero",
      "1" -> "one",
      "2" -> "two",
      "3" -> "three",
      "4" -> "four",
      "5" -> "five",
      "6" -> "six",
      "7" -> "seven",
      "8" -> "eight",
      "9" -> "nine"
    )
    // вариант с заменой по отдельным словам и цифрам
    val words: Array[String] = text.split("\\b")
    words.collect {
      case value if textNumbers.contains(value) => textNumbers(value)
      case word => word
    }.mkString
    // вариант с заменой посимвольно
    /*
    text.toCharArray.collect {
      case ch if ch.isDigit => textNumbers(ch.toString) + " "
      case ch => ch.toString
    }.mkString
    */
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии, что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  case class AutoIgnoreCase(auto: Auto) {
    private val key: (String, String) =
      (auto.mark.toLowerCase, auto.model.toLowerCase)

    // кастомный equals в обёртке AutoIgnoreCase, чтобы Auto сравнивался по ключу независимо от регистра значений полей
    override def equals(obj: Any): Boolean = obj match {
      case other: AutoIgnoreCase => this.key == other.key
      case _ => false
    }

    override def hashCode(): Int = key.hashCode()
  }

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров.
   * Реализуйте метод, который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    // уникальные машины независимо у каждого дилера (убираются дубликаты, которые могли быть по регистру)
    val setOne = dealerOne.map(AutoIgnoreCase.apply).toSet
    val setTwo = dealerTwo.map(AutoIgnoreCase.apply).toSet
    // строим пересечение двух множеств (только общие элементы), которое уберёт дубликаты между двумя дилерами по AutoIgnoreCase,
    // и создаём итоговый список результатов, доставая Auto из AutoIgnoreCase
    (setOne & setTwo).map(_.auto)
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилерском центре, но не обслуживаются во втором.
   * Реализуйте метод, который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    val setOne = dealerOne.map(AutoIgnoreCase.apply).toSet
    val setTwo = dealerTwo.map(AutoIgnoreCase.apply).toSet
    // по условию задачи: нужны элементы первого множества ("обслуживающихся в первом дилерском центре"), которых
    // нет во втором ("и не обслуживающимся во втором") - это и есть определение разности множеств (-- или diff, или &~)
    (setOne -- setTwo).map(_.auto)
  }
}
