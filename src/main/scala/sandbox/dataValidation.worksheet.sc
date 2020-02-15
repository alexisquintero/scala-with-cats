// type Check[A] = A => Either[String, A]
// type Check[E, A] = A => Either[E, A]

import cats.Semigroup
import cats.instances.list._
import cats.syntax.semigroup._

val semigroup = Semigroup[List[String]]
semigroup.combine(List("Badness"), List("More badness"))
List("Oh noes") |+| List("Fail happened")

import cats.syntax.either._
case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(a)) => e.asLeft
        case (Right(a), Left(e)) => e.asLeft
        case (Right(a1), Right(a2)) => a.asRight
      }
    }
}

val a: CheckF[List[String], Int] =
  CheckF { v =>
    if(v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

val b: CheckF[List[String], Int] =
  CheckF { v =>
    if(v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

val check: CheckF[List[String], Int] =
  a and b

check(5)
check(0)
val check2 = a and a
check2(5)
check2(0)

val a2: CheckF[Nothing, Int] =
  CheckF(v => v.asRight)

val b2: CheckF[Nothing, Int] =
  CheckF(v => v.asRight)

// val check3 = a2 and b2

trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(a)) => e.asLeft
          case (Right(a), Left(e)) => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
    }
}

case class And[E, A](
  left: Check[E, A],
  right: Check[E, A]) extends Check[E, A]

case class Pure[E, A](
  func: A => Either[E, A]) extends Check[E, A]

val a3: Check[List[String], Int] =
  Pure { v =>
    if(v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

val b3: Check[List[String], Int] =
  Pure { v =>
    if(v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

val check4: Check[List[String], Int] =
  a3 and b3

check4(5)

import cats.data.Validated
import cats.syntax.apply._
import cats.data.Validated._

trait CheckV[E, A] {
  def and(that: CheckV[E, A]): CheckV[E, A] =
    AndV(this, that)
  def or(that: CheckV[E, A]): CheckV[E, A] =
    OrV(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case PureV(func) =>
        func(a)
      case AndV(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case OrV(left, right) =>
        left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

case class AndV[E, A](
  left: CheckV[E, A],
  right: CheckV[E, A]) extends CheckV[E, A]

case class PureV[E, A](
  func: A => Validated[E, A]) extends CheckV[E, A]

case class OrV[E, A](
  left: CheckV[E, A],
  right: CheckV[E, A]) extends CheckV[E, A]

import cats.syntax.validated._

val a4: CheckV[List[String], Int] =
  PureV { v =>
    if(v > 2) v.valid
    else List("Must be > 2").invalid
  }

val b4: CheckV[List[String], Int] =
  PureV { v =>
    if(v < -2) v.valid
    else List("Must be < -2").invalid
  }

val check5: CheckV[List[String], Int] =
  a4 and b4

check5(5)

val check6: CheckV[List[String], Int] =
  a4 or b4

check6(5)

trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    AndP(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    OrP(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case PureP(func) =>
        func(a)
      case AndP(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case OrP(left, right) =>
        left(a) match {
          case Valid(a1) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

case class AndP[E, A](
  left: Predicate[E, A],
  right: Predicate[E, A]) extends Predicate[E, A]

case class OrP[E, A](
  left: Predicate[E, A],
  right: Predicate[E, A]) extends Predicate[E, A]

case class PureP[E, A](
  func: A => Validated[E, A]) extends Predicate[E, A]

trait Check3[E, A, B] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]
  def map[C](func: B => C): Check3[E, A, C] =
    Map[E, A, B, C](this, func)
  def flatMap[C](func: B => Check3[E, A, C]) =
    FlatMap[E, A, B, C](this, func)
  def andThen[C](that: Check3[E, B, C]): Check3[E, A, C] =
    AndThen[E, A, B, C](this, that)
}

object Check3 {
  def apply[E, A](pred: Predicate[E, A]): Check3[E, A, A] =
    Pure3(pred)
}

case class Map[E, A, B, C](
  check: Check3[E, A, B],
  func: B => C) extends Check3[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

case class Pure3[E, A](
  pred: Predicate[E, A]) extends Check3[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,A] =
      pred(in)
  }

case class FlatMap[E, A, B, C](
  check: Check3[E, A, B],
  func: B => Check3[E, A, C]) extends Check3[E, A, C] {
    def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E,C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

case class AndThen[E, A, B, C](
  check1: Check3[E, A, B],
  check2: Check3[E, B, C]) extends Check3[E, A, C] {
    def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E,C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }
