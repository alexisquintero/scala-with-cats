import cats.Monoid
import cats.syntax.monoid._

def foldMap[A, B: Monoid](values: Vector[A])(fn: A => B): B =
  // values.map(fn).fold(Monoid[B].empty)(_ |+| _)
  values.foldLeft(Monoid[B].empty)(_ |+| fn(_))

import cats.instances.int._

foldMap(Vector(1, 2, 3))(identity)

import cats.instances.string._

foldMap(Vector(1, 2, 3))(_.toString + "! ")
foldMap("Hello world!".toVector)(_.toString.toUpperCase)

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val future1 = Future {
  (1 to 100).toList.foldLeft(0)(_ + _)
}

val future2 = Future {
  (100 to 200).toList.foldLeft(0)(_ + _)
}

val future3 = future1.map(_.toString)

val future4 = for {
  a <- future1
  b <- future2
} yield a + b

Future.sequence(List(Future(1), Future(2), Future(3)))

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

List(Future(1), Future(2), Future(3)).sequence

import scala.concurrent._
import scala.concurrent.duration._

Await.result(Future(1), 1.second)

import cats.Monad

Monad[Future].pure(42)

Monoid[Future[Int]].combine(Future(1), Future(2))

Runtime.getRuntime.availableProcessors

(1 to 10).toList.grouped(3).toList

def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
  val cores: Int = Runtime.getRuntime.availableProcessors
  // val groupSize: Int = if(values.size % cores == 0) values.size / cores else values.size / cores + 1
  val groupSize: Int = (1.0 * values.size / cores).ceil.toInt
  values.
    grouped(groupSize).
    map(group => Future(foldMap(group)(func))).
    reduce(_ |+| _)
}

val result: Future[Int] =
  parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(result, 1.second)

def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
  import cats.instances.vector._
  import cats.syntax.foldable._

  val cores: Int = Runtime.getRuntime.availableProcessors
  val groupSize: Int = (1.0 * values.size / cores).ceil.toInt

  values
    .grouped(groupSize)
    .toVector
    .traverse(group => Future(foldMap(group)(func)))
    .map(_.combineAll)
}
