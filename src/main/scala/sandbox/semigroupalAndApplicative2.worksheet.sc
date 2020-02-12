import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._

type AllErrorsOr[A] = Validated[List[String], A]

Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)

val v = Validated.Valid(123)
val i = Validated.Invalid(List("Badness"))

val v2 = Validated.valid[List[String], Int](123)
val i2 = Validated.invalid[List[String], Int](List("Badness"))

import cats.syntax.validated._

123.valid[List[String]]
List("Badness").invalid[Int]

import cats.syntax.applicative._
import cats.syntax.applicativeError._

type ErrorOr[A] = Validated[List[String], A]

123.pure[ErrorOr]
List("Badness").raiseError[ErrorOr, Int]

Validated.catchOnly[NumberFormatException]("foo".toInt)
Validated.catchNonFatal(sys.error("Badness"))
Validated.fromTry(scala.util.Try("foo".toInt))
Validated.fromEither[String, Int](Left("Badness"))
Validated.fromOption[String, Int](None, "Badness")

type AllErrorsOr2[A] = Validated[String, A]
import cats.instances.string._

Semigroupal[AllErrorsOr2]

import cats.syntax.apply._

(
  "Error 1".invalid[Int],
  "Error 2".invalid[Int]
).tupled

import cats.instances.vector._

(
  Vector(404).invalid[Int],
  Vector(500).invalid[Int]
).tupled

import cats.data.NonEmptyVector

(
  NonEmptyVector.of("Error 1").invalid[Int],
  NonEmptyVector.of("Error 2").invalid[Int]
).tupled

123.valid.map(_ * 100)
"?".invalid.leftMap(_.toString)
123.valid[String].bimap(_ + "!", _ * 100)
"?".invalid[String].bimap(_ + "!", _ * 100)

32.valid.andThen { a =>
  10.valid.map { b =>
    a + b
  }
}

import cats.syntax.either._

"Badness".invalid[Int]
"Badness".invalid[Int].toEither
"Badness".invalid[Int].toEither.toValidated

123.valid[Int].ensure("Negative!")(_ > 0)
-123.valid[Int].ensure("Negative!")(_ > 0)

"fail".invalid[Int].getOrElse(0)
"fail".invalid[Int].fold(_ + "!!!", _.toString)

// 6.4.4 Exercise: Form Validation

case class User(name: String, age: Int)

import cats.data.Validated

type FormData = Map[String, String]
type FailFast[A] = Either[List[String], A]
type FailSlow[A] = Validated[List[String], A]

def getValue(name: String)(data: FormData): FailFast[String] = {
  data.get(name).
    toRight(List(s"$name field not specified"))
}

val getName = getValue("name") _
getName(Map("name" -> "Dade Murphy"))
getName(Map())

type NumFmtExn = NumberFormatException

def parseInt(name: String)(data: String): FailFast[Int] = {
  Either.catchOnly[NumFmtExn](data.toInt).
    leftMap(_ => List(s"$name must be an integer"))
}

parseInt("age")("11")
parseInt("age")("foo")

def nonBlank(name: String)(data: String): FailFast[String] =
  Right(data).
    ensure(List(s"$name cannot be blank"))(_.nonEmpty)


def nonNegative(name: String)(data: Int): FailFast[Int] =
  Right(data).
    ensure(List(s"$name must be non-negative"))(_ >= 0)

nonBlank("name")("Dade Murphy")
nonBlank("name")("")

nonNegative("age")(11)
nonNegative("age")(-1)

def readName[A](data: FormData): FailFast[String] = {
  getValue("name")(data).
    flatMap(nonBlank("name"))
}

def readAge(data: FormData): FailFast[Int] = {
  getValue("age")(data).
    flatMap(nonBlank("age")).
    flatMap(parseInt("age")).
    flatMap(nonNegative("age"))
}

readName(Map("name" -> "Dade Murphy"))
readName(Map("name" -> ""))
readName(Map())

readAge(Map("age" -> "11"))
readAge(Map("age" -> "-1"))
readAge(Map())

def readUser(data: FormData): FailSlow[User] = {
  import cats.instances.list._
  import cats.syntax.apply._

  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply)
}

readUser(Map("name" -> "Dave", "age" -> "37"))
readUser(Map("age" -> "-1"))
