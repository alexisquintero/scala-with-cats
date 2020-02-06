// import scala.concurrent.ExecutionContext
List(1, 2, 3).map(n => n + 1)

List(1, 2, 3).
  map(n => n + 1).
  map(n => n * 2).
  map(n => n + "!")

import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val future: Future[String] =
  Future(123).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!")

Await.result(future, 1.second)

import scala.util.Random

val future1 = {
  val r =  new Random(0L)
  val x = Future(r.nextInt)

  for {
    a <- x
    b <- x
  } yield (a, b)
}

val future2 = {
  val r =  new Random(0L)

  for {
    a <- Future(r.nextInt)
    b <- Future(r.nextInt)
  } yield (a, b)
}

val result1 = Await.result(future1, 1.second)
val result2 = Await.result(future2, 1.second)

import cats.instances.function._
import cats.syntax.functor._

val func1: Int => Double =
  (x: Int) => x.toDouble

val func2: Double => Double =
  (y: Double) => y * 2

(func1 map func2)(1)
(func1 andThen func2)(1)
func2(func1(1))

// import scala.language.higherKinds
import cats.Functor
import cats.instances.list._
import cats.instances.option._

val list1 = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
val option1 = Option(123)
val option2 = Functor[Option].map(option1)(_.toString)

val func = (x: Int) => x + 1
val liftedFunc = Functor[Option].lift(func)
liftedFunc(Option(1))

val func12 = (a: Int) => a + 1
val func22 = (a: Int) => a * 2
val func3 = (a: Int) => a + "!"
val func4 = func12.map(func22).map(func3)

func4(123)

def doMath[F[_]: Functor](start: F[Int]): F[Int] =
  start.map(n => n + 1 * 2)

doMath(Option(20))
doMath(List(1, 2, 3))

implicit val optionFunctor: Functor[Option] =
  new Functor[Option] {
    def map[A, B](value: Option[A])(func: A => B): Option[B] =
      value.map(func)
  }

// implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
//   new Functor[Future] {
//     def map[A, B](value: Future[A])(func: A => B): Future[B] =
//       value.map(func)
//   }

// 3.5.4 Exercise: Branching out with Functors

sealed trait Tree[+A]
// final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
// final case class Leaf[A](value: A) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

implicit val treeFunctor = new Functor[Tree] {
  def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

// Branch(Leaf(10), Leaf(20)).map(_ * 2)
Tree.leaf(100).map(_ * 2)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

trait Printable[A] {
  self =>

  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
}

def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String = "\"" + value + "\""
  }

implicit val booleanPrintable: Printable[Boolean] =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

format("hello")
format(true)

// final case class Box[A](value: A)
case class Box[A](value: A)

// implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
//   new Printable[Box[A]] {
//     def format(value: Box[A]): String = p.format(value.value)
//   }

implicit def boxPrintable[A](implicit p: Printable[A]) =
  p.contramap[Box[A]](_.value)

format(Box("hello world"))
format(Box(true))
// format(Box(123))

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))
      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

implicit val intCodec: Codec[Int] =
  stringCodec.imap(_.toInt, _.toString)

implicit val booleanCodec: Codec[Boolean] =
  stringCodec.imap(_.toBoolean, _.toString)

implicit val doubleCodec: Codec[Double] =
  stringCodec.imap(_.toDouble, _.toString)

implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
  c.imap[Box[A]](Box(_), _.value)

encode(123.4)
decode[Double]("123.4")
encode(Box(123.4))
decode[Box[Double]]("123.4")

import cats.Contravariant
import cats.Show
import cats.instances.string._

val showString = Show[String]

val showSymbol = Contravariant[Show].
  contramap(showString)((sym: Symbol) => s"'${sym.name}")

showSymbol.show('dave)

import cats.syntax.contravariant._

showString.contramap[Symbol](_.name).show('dave)

import cats.Monoid
import cats.syntax.invariant._
import cats.syntax.semigroup._

implicit val symbolMonoid: Monoid[Symbol] =
  Monoid[String].imap(Symbol.apply)(_.name)

Monoid[Symbol].empty

'a |+| 'few |+| 'words

val func3a: Int => Double =
  a => func2(func1(a))

val func3b: Int => Double =
  func2.compose(func1)

// val func3c: Int => Double =
//   func2.contramap(func1)

// type <=[B, A] = A => B
// type F[A] = Double <= A

// val func2b: Double <= Double = func2

// val func3c = func2b.contramap(func1)
