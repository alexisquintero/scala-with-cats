import cats.Monad
import cats.instances.option._
import cats.instances.list._

val opt1 = Monad[Option].pure(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
val opt3 = Monad[Option].map(opt2)(a => 100 * a)
val list1 = Monad[List].pure(3)
val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))
val list3 = Monad[List].map(list2)(a => a + 123)

import cats.instances.vector._

Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

import cats.instances.future._
import scala.concurrent._
// import scala.concurrent.duration._

// val fm = Monad[Future]

import scala.concurrent.ExecutionContext.Implicits.global

val fm = Monad[Future]
val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

import cats.syntax.applicative._

1.pure[Option]
1.pure[List]

import cats.syntax.functor._
import cats.syntax.flatMap._

def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  a.flatMap(x => b.map(y => x*x + y*y))

sumSquare(Option(3), Option(4))
sumSquare(List(1, 2, 3), List(4, 5))

def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x*x + y*y

sumSquare2(Option(3), Option(4))
sumSquare2(List(1, 2, 3), List(4, 5))

// sumSquare2(3, 4)

import cats.Id

sumSquare2(3: Id[Int], 4: Id[Int])

"Dave": Id[String]
123: Id[Int]
List(1, 2, 3): Id[List[Int]]

val a = Monad[Id].pure(3)
val b = Monad[Id].flatMap(a)(_ + 1)

for {
  x <- a
  y <- b
} yield x + y

def pure[A](value: A): Id[A] = value
pure(123)
def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)
map(123)(_ * 2)
def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
flatMap(123)(_ * 2)

val either1: Either[String, Int] = Right(10)
val either2: Either[String, Int] = Right(32)

for {
  a <- either1.right
  b <- either2.right
} yield a + b

for {
  a <- either1
  b <- either2
} yield a + b

// import cats.syntax.either._

// for {
//   a <- either1
//   b <- either2
// } yield a + b

import cats.syntax.either._

val a2 = 3.asRight[String]
val b2 = 4.asRight[String]

for {
  x <- a
  y <- b
} yield x*x + y*y

// def countPositive(nums: List[Int]) =
//   nums.foldLeft(Right(0)) { (accumulator, num) =>
//     if(num > 0) {
//       accumulator.map(_ + 1)
//     } else {
//       Left("Negative. Stopping!")
//     }
//   }

def countPositive(nums: List[Int]) =
  nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
    if(num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    }
  }

countPositive(List(1, 2, 3))
countPositive(List(1, -2, 3))

Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Badness"))
Either.fromTry(scala.util.Try("foo".toInt))
Either.fromOption[String, Int](None, "Badness")

"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])

-1.asRight[String].ensure("Must be non-negative!")(_ > 0)

"error".asLeft[Int].recover {
  case str: String => -1
}

"error".asLeft[Int].recoverWith {
  case str: String => Right(-1)
}

"foo".asLeft[Int].leftMap(_.reverse)
6.asRight[String].bimap(_.reverse, _ * 7)
"bar".asLeft[Int].bimap(_.reverse, _ * 7)

123.asRight[String]
123.asRight[String].swap

for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if(b == 0) "DIV0".asLeft[Int]
       else (a / b).asRight[String]
} yield c * 100

sealed trait LoginError extends Product with Serializable
// final case class UserNotFound(username: String) extends LoginError
case class UserNotFound(username: String) extends LoginError
// final case class PasswordIncorrect(username: String) extends LoginError
case class PasswordIncorrect(username: String) extends LoginError

case object UnexpectedError extends LoginError

case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(u) =>
      println(s"User not found: $u")
    case PasswordIncorrect(u) =>
      println(s"Password incorrect: $u")
    case UnexpectedError =>
      println(s"Unexpected error")
  }

val result1: LoginResult = User("dave", "password").asRight
val result2: LoginResult = UserNotFound("dave").asLeft
result1.fold(handleError, println)
result2.fold(handleError, println)

// trait MonadError[F[_], E] extends Monad[F] {
//   def raiseError[A](e: E): F[A]
//   def handleError[A](fa: F[A])(f: E => A): F[A]
//   def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
// }

import cats.MonadError
import cats.instances.either._

type ErrorOr[A] = Either[String, A]

val monadError = MonadError[ErrorOr, String]

val success = monadError.pure(42)
val success2 = monadError.pure("Badness")
val failure = monadError.raiseError("Badness")

monadError.ensure(success)("Number too low!")(_ > 1000)

import cats.syntax.applicative._
import cats.syntax.applicativeError._
// import cats.syntax.monadError._

val success3 = 42.pure[ErrorOr]
val failure2 = "Badness".raiseError[ErrorOr, Int]
success3.ensure("Number too low!")(_ > 1000)

import scala.util.Try
import cats.instances.try_._

val exn: Throwable = new RuntimeException("It's all gone wrong")
exn.raiseError[Try, Int]
