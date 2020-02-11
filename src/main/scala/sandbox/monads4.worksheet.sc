import cats.data.Writer

Writer(Vector(
  "It was the best of times",
  "it was the worst of times"
), 1959)

import cats.syntax.applicative._
import cats.instances.vector._

type Logged[A] = Writer[Vector[String], A]

123.pure[Logged]

import cats.syntax.writer._

Vector("msg1", "msg2", "msg3").tell

val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
val b = 123.writer(Vector("msg1", "msg2", "msg3"))

val aResult: Int = a.value
val aLog: Vector[String] = a.written

val (log, result) = b.run

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

val writer3 = writer1.bimap(
  log => log.map(_.toUpperCase),
  res => res * 100
)

writer3.run

val writer4 = writer1.mapBoth { (log, res) =>
  val log2 = log.map(_ + "!")
  val res2 = res * 1000
  (log2, res2)
}

writer4.run

val writer5 = writer1.reset

writer5.run

val writer6 = writer1.swap

writer6.run

// 4.7.3 Exercise: Show Your Working

def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Int = {
  val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
  println(s"fact $n $ans")
  ans
}

factorial(5)

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

Await.result(Future.sequence(Vector(
  Future(factorial(3)),
  Future(factorial(3))
)), 5.seconds)

def factorialWriter(w: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
  val n = slowly(if(w.value == 0) 1.pure[Logged] else factorialWriter(w.map(_ - 1)).map(_ * w.value))
  n.tell(Vector(s"fact ${w.value} ${n.value}"))
}

def factorial2(n: Int): Writer[Vector[String], Int] = {
  factorialWriter(n.pure[Logged])
}

Await.result(Future.sequence(Vector(
  Future(factorial2(3)),
  Future(factorial2(3))
)), 5.seconds)

def factorial3(n: Int): Logged[Int] =
  for {
    ans <- if (n == 0) {
             1.pure[Logged]
           } else {
             slowly(factorial3(n - 1).map(_ * n))
           }
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

val (log2, res) = factorial3(5).run

def factorialWriter2(w: Int): Writer[Vector[String], Int] = {
  slowly(
    if(w == 0) 1.pure[Logged]
    else factorialWriter2(w - 1).map(_ * w)
  ).mapBoth { (log, res) =>
    (log ++ Vector(s"fact $w $res"), res)
  }
}

val (l, r) = factorialWriter2(5).run
