def show[A](list: List[A]): String =
  list.foldLeft("nil")((accum, item) => s"$item then $accum")

show(Nil)
show(List(1, 2, 3))

List(1, 2, 3).foldLeft(0)(_ + _)
List(1, 2, 3).foldRight(0)(_ + _)

List(1, 2, 3).foldLeft(0)(_ - _)
List(1, 2, 3).foldRight(0)(_ - _)

// 7.1.2 Exercise: Reflecting on Folds

List(1, 2, 3).foldLeft(List.empty[Int])((acc, item) => item :: acc)
List(1, 2, 3).foldRight(List.empty[Int])((item, acc) => item :: acc)
// List(1, 2, 3).foldRight(Nil)(_ :: _)

def map[A, B](fa: List[A])(f: A => B): List[B] =
  fa.foldRight(List.empty[B])((item, acc) => f(item) :: acc)

map(List(1, 2, 3))(_ * 2)

def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
  fa.foldRight(List.empty[B])((item, acc) => f(item) ++ acc)

flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

def filter[A](list: List[A])(func: A => Boolean): List[A] =
  list.foldRight(List.empty[A])((item, acc) => if(func(item)) item :: acc else acc)

filter(List(1, 2, 3))(_ % 2 == 1)

import cats.Monoid

def sum[A: Monoid](list: List[A]): A = {
  import cats.syntax.monoid._

  list.foldRight(Monoid[A].empty)((item, acc) => item |+| acc)
}

import cats.instances.int._

sum(List(1, 2, 3))

import cats.Foldable
import cats.instances.list._

val ints = List(1, 2, 3)

Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._

val maybeInt = Option(123)

Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

import cats.Eval
import cats.Foldable

def bigData = (1 to 100000).toStream

// bigData.foldRight(0L)(_ + _) // StackOverflow

import cats.instances.stream._

val eval: Eval[Long] =
  Foldable[Stream].
    foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }

eval.value

(1 to 100000).toList.foldRight(0L)(_ + _)
(1 to 100000).toVector.foldRight(0L)(_ + _)

Foldable[Option].nonEmpty(Option(42))
Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)

Foldable[List].combineAll(List(1, 2, 3))

import cats.instances.string._

Foldable[List].foldMap(List(1, 2, 3))(_.toString)

import cats.instances.vector._

val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))

(Foldable[List] compose Foldable[Vector]).combineAll(ints2)

import cats.syntax.foldable._

List(1, 2, 3).combineAll
List(1, 2, 3).foldMap(_.toString)

List(1, 2, 3).foldLeft(0)(_ + _)

def sum2[F[_]: Foldable](values: F[Int]): Int =
  values.foldLeft(0)(_ + _)

sum2(List(1, 2, 3))
