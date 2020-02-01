sealed trait Json
// final case class JsObject(get: Map[String, Json]) extends Json
case class JsObject(get: Map[String, Json]) extends Json
// final case class JsString(get: String) extends Json
case class JsString(get: String) extends Json
// final case class JsNumber(get: Double) extends Json
case class JsNumber(get: Double) extends Json
case object JsNull extends Json

trait JsonWriter[-A] {
  def write(value: A): Json
}

// final case class Person(name: String, email: String)
case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

import JsonWriterInstances._

Json.toJson(Person("Dave", "dave@example.com"))

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

import JsonSyntax._

"asd".toJson
Person("Dave", "dave@example.com").toJson

implicitly[JsonWriter[String]]

Json.toJson("A string!")

// implicit val writer1: JsonWriter[String] =
//   JsonWriterInstances.stringWriter

// implicit val writer2: JsonWriter[String] =
//   JsonWriterInstances.stringWriter

implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
  new JsonWriter[Option[A]] {
    def write(option: Option[A]): Json =
      option match {
        case Some(value) => writer.write(value)
        case None => JsNull
      }
  }

Json.toJson(Option("A string"))

// implicit def optionWriter2[A](writer: JsonWriter[A]): JsonWriter[Option[A]] = ???

// 1.3 Exercise: Printable Library

trait Printable[A] {
  def format(a: A): String
}

case object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(a: String) = a
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(a: Int): String = a.toString
  }
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)
  def print[A](a: A)(implicit printable: Printable[A]): Unit = println(format(a))
}

case class Cat(name: String, age: Int, color: String)

object Cat {
  import PrintableInstances._
  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.int._
  import cats.instances.string._

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    def format(cat: Cat): String = {
      val name: String = Printable.format(cat.name)
      val age: String = Printable.format(cat.age)
      val color: String = Printable.format(cat.color)
      s"$name is a $age year-old $color cat"
    }
  }

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.age === cat2.age && cat1.name === cat2.name && cat1.color === cat2.color
    }
}

val cat = Cat("Garfield", 38, "ginger and black")
Printable.format(cat)

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = Printable.format(a)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}

import PrintableSyntax._

cat.format

// import java.util.Date

// new Date().print

import cats.Show

// val showInt = Show.apply[Int]

import cats.instances.int._
import cats.instances.string._

val showInt: Show[Int] = Show.apply[Int]
val showString: Show[String] = Show.apply[String]

val intAsString: String = showInt.show(123)
val stringAsString: String = showString.show("abc")

import cats.syntax.show._

val shownInt = 123.show
val shownString = "abc".show

import java.util.Date

// implicit val dateShow: Show[Date] =
//   new Show[Date] {
//     def show(date: Date): String =
//       s"${date.getTime}ms since the epoch."
//   }

implicit val dateShow: Show[Date] =
  Show.show(date => s"${date.getTime}ms since the epoch.")

implicit val catShow: Show[Cat] =
  Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")

cat.show

import cats.Eq
import cats.instances.int._

val eqInt = Eq[Int]

eqInt.eqv(123, 123)
eqInt.eqv(123, 234)
// eqInt.eqv(123, "234")

import cats.syntax.eq._

123 === 123
123 =!= 234
// 123 === "123"

import cats.instances.option._

// Some(1) === None
(Some(1): Option[Int]) === (None: Option[Int])
Option(1) === Option.empty[Int]

import cats.syntax.option._

1.some === none[Int]
1.some =!= none[Int]

import cats.instances.long._

implicit val dateEq: Eq[Date] =
  Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }

val x = new Date()
val y = new Date()

 x === x
 x === y

val cat1 = Cat("Garfield",   38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2
cat1 =!= cat2

optionCat1 === optionCat2
optionCat1 =!= optionCat2
