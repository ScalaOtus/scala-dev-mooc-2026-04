package ru.otus.module2

//import ru.otus.module2.type_classes.Eq.{given_Eq_String, *}
//import ru.otus.module2.type_classes.{JsValue, toJson}
//import ru.otus.module2.type_classes.JsValue.{JsNull, JsNumber, JsString}
import ru.otus.module2.homework06.{JsValue, toJson}
import ru.otus.module2.homework06.JsValue.{JsNull, JsNumber, JsString}
import ru.otus.module2.homework06.JsonSyntax.ToJsonOps


/*
object type_classes {

  sealed trait JsValue

  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue

    final case class JsString(get: String) extends JsValue

    final case class JsNumber(get: Double) extends JsValue

    case object JsNull extends JsValue
  }
  
  // 1
  
  trait JsonWriter[T] {
    def toJson(v: T): JsValue
  }
  
  object JsonWriter {
    
    def apply[T](using ev: JsonWriter[T]) = ev
    
    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
        override def toJson(v: T): JsValue = f(v)
    }
    
    given JsonWriter[String] = from[String](JsString)

    given JsonWriter[Int] = from[Int](JsNumber)
    
    given optJson [T](using jw: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
      case Some(value) => jw.toJson(value)
      case None => JsNull
    }
  }

  def toJson[T: JsonWriter](v: T): JsValue = JsonWriter[T].toJson(v)



  // 1 type constructor
  trait Ordering[T]:
    def less(a: T, b: T): Boolean

  object Ordering {

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }

    given Ordering[Int] = from[Int](_ < _)

    given Ordering[String] = from[String](_ < _)

    given Ordering[User] = from[User](_.age < _.age)
  }

  case class User(name: String, age: Int)


  def greatest[A](a: A, b: A)(using ord: Ordering[A]): A =
    if(ord.less(a, b)) b else a


  greatest(5, 10)
//  greatest("ab", "abcd")
  greatest(User("Bob", 16), User("Alice", 18))

    
  trait Eq[T]{
    extension (a: T) def ===(b: T): Boolean
  }
  
  object Eq {
    given Eq[String] = new Eq[String] {
      extension (a: String) override def ===(b: String): Boolean = a == b
    }
  } 
    
//  extension [T](a: T)(using eq: Eq[T]){
//    def ===(b: T): Boolean = eq.===(a, b)
//  }

  val result = List("a", "b", "c").filter(str => str === "1")

}
*/


// Домашнее задание
//  1. Переписать JsValue и JsonWriter из занятия про implicits на Scala 2
//  2. Максимально использовать конструкции и синтаксис Scala 2

object homework06 {

  sealed trait JsValue

  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    case object JsNull extends JsValue
  }

  trait JsonWriter[T] {
    def toJson(v: T): JsValue
  }

  object JsonWriter {
    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
      override def toJson(v: T): JsValue = f(v)
    }

    implicit val jsonWriterString: JsonWriter[String] = from[String](JsString)
    implicit val jsonWriterInt: JsonWriter[Int] = from[Int](JsNumber)

    implicit def optJson[T](implicit jw: JsonWriter[T]): JsonWriter[Option[T]] =
      from[Option[T]] {
        case Some(value) => jw.toJson(value)
        case None => JsNull
      }
  }

  // В Scala 2 нужен implicit class, чтобы сделать extension‑метод для implicit conversion
  object JsonSyntax {
    implicit class ToJsonOps[T](private val value: T) extends AnyVal {
      def toJson(implicit ev: JsonWriter[T]): JsValue =
        JsonWriter[T].toJson(value)
    }
  }

  def toJson[T: JsonWriter](v: T): JsValue = JsonWriter[T].toJson(v)

  def main(args: Array[String]): Unit = {
    // T
    println("String.toJson -> " + "test_string".toJson)   // JsString(test_string)
    println("Int.toJson -> " + 10.toJson)           // JsNumber(10.0)

    println("\ntoJson(String) -> " + toJson("hello")) // JsString("hello")
    println("toJson(Int) -> " + toJson(30))         // JsNumber(30.0)

    // Option[T]
    val someEmpty: Option[Int] = None
    println("\nOption(Int).toJson -> " + Option(10).toJson)
    println("Option(String).toJson -> " + Option("test_option").toJson)
    println("\nOption(None).toJson -> " + someEmpty.toJson)   // JsNull
    println("toJson(None) -> " + toJson(someEmpty))         // JsNull

    // Пример с составным типом (объект через Map)
    val user = Map(
      "name" -> Some("Alice"),
      "age" -> Some(30),
      "city" -> None
    )

    // Для Map[String, Option[JsValue]] нужно собрать JsObject
    val jsUser = JsValue.JsObject(Map(
      "name" -> toJson("Alice"),
      "age" -> toJson(30),
      "city" -> JsNull
    ))

    println("User object:")
    println(jsUser)   // JsObject(Map(name -> JsString(Alice), age -> JsNumber(30.0), city -> JsNull))
  }

}

