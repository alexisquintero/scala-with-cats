import cats.data.Kleisli
import cats.instances.list._

val step1: Kleisli[List, Int, Int] =
  Kleisli(x => List(x + 1, x - 1))

val step2: Kleisli[List, Int, Int] =
  Kleisli(x => List(x, -x))

val step3: Kleisli[List, Int, Int] =
  Kleisli(x => List(x * 2, x / 2))

val pipeline = step1 andThen step2 andThen step3

pipeline.run(20)

val pipeline2 = for {
  f1 <- List[Int => Int](_ + 1, _ -1).toList
  f2 <- List[Int => Int](identity _, _ * -1)
  f3 <- List[Int => Int](_ * 2, _ / 2)
} yield f1 andThen f2 andThen f3

pipeline2.map(_.apply(20))

import cats.Semigroup
import cats.data.Validated
import cats.syntax.apply._
import cats.data.Validated._
import cats.syntax.monoid._
import cats.syntax.validated._

trait Predicate[E, A] {
  import Predicate._

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this(a).toEither

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) =>
        left(a) match {
          case Valid(a1)   => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2)   => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

object Predicate {
  case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  case class Pure[E, A](
    func: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if(fn(a)) a.valid else err.invalid)
}

import cats.data.NonEmptyList

type Errors = NonEmptyList[String]
type Result[A] = Either[Errors, A]
type Check[A, B] = Kleisli[Result, A, B]

def error(s: String): NonEmptyList[String] =
  NonEmptyList(s, Nil)

def check[A, B](func: A => Result[B]): Check[A, B] =
  Kleisli(func)

def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
  Kleisli[Result, A, A](pred.run)

def longerThan(n: Int): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be longer than $n characters"),
    str => str.size > n)

def alphanumeric: Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit))

def contains(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char))

def containsOnce(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.filter(c => c == char).size == 1)

val checkUsername: Check[String, String] =
  checkPred(longerThan(3) and alphanumeric)

val splitEmail: Check[String, (String, String)] =
  check(_.split('@') match {
    case Array(name, domain) =>
      Right((name, domain))
    case other =>
      Left(error("Must contain a single @ character"))
  })

val checkLeft: Check[String, String] =
  checkPred(longerThan(0))

val checkRight: Check[String, String] =
  checkPred(longerThan(3) and contains('.'))

import cats.instances.either._

val joinEmail: Check[(String, String), String] =
  check {
    case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
  }

val checkEmail: Check[String, String] =
  splitEmail andThen joinEmail

case class User(username: String, email: String)

def createUser(
  username: String,
  email: String): Either[Errors, User] = (
    checkUsername.run(username),
    checkEmail.run(email)
  ).mapN(User)

createUser("Noel", "noel@underscore.io")
createUser("", "dave@underscore@io")
