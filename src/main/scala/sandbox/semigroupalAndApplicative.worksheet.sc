import cats.syntax.either._

def parseInt(str: String): Either[String, Int] =
  Either.catchOnly[NumberFormatException](str.toInt).
    leftMap(_ => s"Could't read $str")

for {
  a <- parseInt("a")
  b <- parseInt("b")
  c <- parseInt("c")
} yield (a + b + c)

// trait Semigroupal[F[_]] {
//   def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
// }

import cats.Semigroupal
import cats.instances.option._

Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))
Semigroupal[Option].product(Some(123), None)

Semigroupal.tuple3(Option(1), Option(2), Option(3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)

import cats.syntax.apply._

(Option(123), Option("abc")).tupled
(Option(123), Option("abc"), Option(true)).tupled

case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & black")
).mapN(Cat.apply)

val add: (Int, Int) => Int = (a, b) => a + b
// (Option(1), Option(2), Option(3)).mapN(add)
// (Option("cats"), Option(true)).mapN(add)

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._

case class Cat2(
  name: String,
  yearOfBirth: Int,
  favoriteFoods: List[String]
)

val tupleToCat2: (String, Int, List[String]) => Cat2 =
  Cat2.apply _

val cat2ToTuple: Cat2 => (String, Int, List[String]) =
  cat2 => (cat2.name, cat2.yearOfBirth, cat2.favoriteFoods)

implicit val cat2Monoid: Monoid[Cat2] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
).imapN(tupleToCat2)(cat2ToTuple)

import cats.syntax.semigroup._

val garfield = Cat2("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))

garfield |+| heathcliff

import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
// import scala.language.higherKinds

val futurePair = Semigroupal[Future].
  product(Future("Hello"), Future(123))

val futureCat2 = (
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
).mapN(Cat2.apply)

Await.result(futureCat2, 1.second)

Semigroupal[List].product(List(1, 2), List(3, 4))

import cats.instances.either._

type ErrorOr[A] = Either[Vector[String], A]

Semigroupal[ErrorOr].product(
  Left(Vector("Error 1")),
  Left(Vector("Error 2"))
)

// 6.3.1.1 Exercise: The Product of Monads
import cats.Monad

def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  x.flatMap(a => y.map(b => (a, b)))

  // for {
  //   a <- x
  //   b <- y
  // } yield (a, b)
}
