sealed trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

import cats.Monad
// import scala.annotation.tailrec

implicit val treeMonad = new Monad[Tree] {
  def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
    case Leaf(value) => f(value)
    case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
  }

  def pure[A](x: A): Tree[A] = Leaf(x)

  def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = flatMap(f(a)) {
    case Left(value) => tailRecM(value)(f)
    case Right(value) => Leaf(value)
  }
}

import cats.syntax.functor._
import cats.syntax.flatMap._

branch(leaf(100), leaf(200)).
  flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
  c <- branch(leaf(b - 1), leaf(b + 1))
} yield c
