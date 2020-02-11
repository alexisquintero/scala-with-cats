// import cats.Monad
import cats.syntax.applicative._
// import cats.syntax.flatMap._
// import scala.language.higherKinds

// def compose[M1[_]: Monad, M2[_]: Monad] = {
//   type Composed[A] = M1[M2[A]]

//   new Monad[Composed] {
//     def pure[A](a: A): Composed[A] =
//       a.pure[M2].pure[M1]

//     def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = ???
//   }
// }

import cats.data.OptionT

type ListOption[A] = OptionT[List, A]

import cats.instances.list._

val result1: ListOption[Int] = OptionT(List(Option(10)))

val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

type ErrorOr[A] = Either[String, A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._

val a = 10.pure[ErrorOrOption]
val b = 32.pure[ErrorOrOption]
val c = a.flatMap(x => b.map(y => x + y))

// case class EitherT[F[_], E, A](stack: F[Either[E, A]]) {
// }

import scala.concurrent.Future
import cats.data.EitherT

type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
val errorStack2 = 32.pure[ErrorOrOption]

errorStack1.value
errorStack2.value

futureEitherOr
val intermediate = futureEitherOr.value
val stack = intermediate.value
Await.result(stack, 1.second)

sealed abstract class HttpError
// final case class NotFound(item: String) extends HttpError
case class NotFound(item: String) extends HttpError
// final case class BadRequest(msg: String) extends HttpError
case class BadRequest(msg: String) extends HttpError

type FutureEither2[A] = EitherT[Future, HttpError, A]

import cats.data.Writer

type Logged[A] = Writer[List[String], A]

def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"read $str"), Some(num))
    case None => Writer(List(s"Failed on $str"), None)
  }

def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
  // import cats.data.optionT

  val result = for {
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  } yield a + b + c

  result.value
}

val result3 = addAll("1", "2", "3")
val result4 = addAll("1", "a", "3")
