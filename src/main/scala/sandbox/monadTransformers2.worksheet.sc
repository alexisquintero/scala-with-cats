// 5.4 Exercise: Monads: Transformation and Roll Out
import scala.concurrent.Future

// type Response[A] = Future[Either[String, A]]

// def getPowerLevel(autobot: String): Response[Int] = ???

import cats.data.EitherT

type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz" -> 6,
  "Bumblebee" -> 8,
  "Hot Rod" -> 10
)

import cats.instances.future._
// import cats.syntax.flatMap._
import scala.concurrent.ExecutionContext.Implicits.global

def getPowerLevel(ally: String): Response[Int] =
  powerLevels.get(ally) match {
    case Some(value) => EitherT.right(Future(value))
    case None => EitherT.left(Future(s"$ally unreachable"))
  }

getPowerLevel("Jazz").value

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    a1 <- getPowerLevel(ally1)
    a2 <- getPowerLevel(ally2)
  } yield (a1 + a2) > 15

canSpecialMove("Jazz", "Hot Rod").value
canSpecialMove("Jazz", "Jazz").value

def tacticalReport(ally1: String, ally2: String): String = {
  import scala.concurrent.Await
  import scala.concurrent.duration._

  Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
    case Left(value) =>
      s"Comms error: $value"
    case Right(true) =>
      s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) =>
      s"$ally1 and $ally2 need a recharge!"
  }
}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")
