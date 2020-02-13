import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUpTime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUpTime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
  }

Await.result(allUptimes, 1.second)

val allUptimes2: Future[List[Int]] =
  Future.traverse(hostnames)(getUpTime)

Await.result(allUptimes2, 1.second)

import cats.Applicative
import cats.instances.future._
import cats.syntax.applicative._

List.empty[Int].pure[Future]

import cats.syntax.apply._

def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
  (accum, getUpTime(host)).mapN(_ :+ _)

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

val totalUptime = listTraverse(hostnames)(getUpTime)
Await.result(totalUptime, 1.second)

import cats.instances.vector._

// 7.2.2.1 Exercise: Traversing with Vectors
listSequence(List(Vector(1, 2), Vector(3, 4)))
listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

// 7.2.2.2 Exercise: Traversing with Options

import cats.instances.option._

def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

process(List(2, 4, 6))
process(List(1, 2, 3))

// 7.2.2.3 Exercise: Traversing with Validated

import cats.data.Validated
import cats.instances.list._

type ErrorsOr[A] = Validated[List[String], A]

def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if(n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }

process2(List(2, 4, 6))
process2(List(1, 2, 3))

import cats.Traverse
import cats.instances.future._

val totalUptime2: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUpTime)

Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))
val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._

Await.result(hostnames.traverse(getUpTime), 1.second)
numbers.sequence
Await.result(numbers.sequence, 1.second)
