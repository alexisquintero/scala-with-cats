import cats.Monoid
import cats.instances.string._

Monoid[String].combine("Hi ", "there")
Monoid[String].empty

Monoid.apply[String].combine("Hi ", "there")
Monoid.apply[String].empty

import cats.Semigroup

Semigroup[String].combine("Hi ", "there")

import cats.instances.int._

Monoid[Int].combine(32, 10)

import cats.instances.option._

val a = Option(22)
val b = Option(20)

Monoid[Option[Int]].combine(a, b)

import cats.syntax.semigroup._

val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
val intResult = 1 |+| 2 |+| Monoid[Int].empty

def add(items: List[Int]): Int =
  items.foldLeft(0)(_ + _)

def add2(items: List[Int]): Int =
  items.foldLeft(Monoid[Int].empty)(_ |+| _)

def add3[A: Monoid](items: List[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

add3(List(1, 2, 3))
add3(List(Some(1), Some(3), None))
// add3(List(Some(1), Some(3)))

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
  import cats.instances.double._

  def combine(x: Order, y: Order): Order = {
    val cost = add3(List(x.totalCost, y.totalCost))
    val quantity = add3(List(x.quantity, y.quantity))
    Order(cost, quantity)
  }
  def empty: Order = Order(0, 0)
}

val order1 = Order(1, 1)
val order2 = Order(2, 2)

add3(List(order1, order2))
