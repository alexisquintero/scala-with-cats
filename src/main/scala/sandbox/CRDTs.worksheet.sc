// case class GCounter(counters: Map[String, Int]) {
//   def increment(machine: String, amount: Int): GCounter = {
//     val newValue: Int = counters.getOrElse(machine, 0) + amount
//     GCounter(counters + (machine -> newValue))
//   }

//   def merge(that: GCounter): GCounter = {
//     val newCounters: Map[String, Int] = that.counters ++ counters map {
//       case (k, v) =>
//         k -> (v max that.counters.getOrElse(k, 0))
//     }
//     GCounter(newCounters)
//   }

//   def total: Int =
//     counters.values.sum
// }

import cats.kernel.CommutativeMonoid

trait BoundedSemilatice[A] extends CommutativeMonoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object BoundedSemilatice {

  implicit val intInstance: BoundedSemilatice[Int] =
    new BoundedSemilatice[Int] {
      def combine(x: Int, y: Int): Int = x max y
      def empty: Int = 0
    }

  implicit def setInstance[A]: BoundedSemilatice[Set[A]] =
    new BoundedSemilatice[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      def empty: Set[A] = Set.empty[A]
    }
}

// case class GCounter[A](counters: Map[String, A]) {
//   import cats.syntax.monoid._
//   import cats.instances.map._
//   import cats.instances.list._
//   import cats.syntax.foldable._

//   def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
//     val value: A = amount |+| counters.getOrElse(machine, m.empty)
//     GCounter(counters + (machine -> value))
//   }

//   def merge(that: GCounter[A])(implicit m: BoundedSemilatice[A]): GCounter[A] =
//     GCounter(this.counters |+| that.counters)

//   def total(implicit m: CommutativeMonoid[A]): A =
//     counters.values.toList.combineAll
// }

// val m1 = Map("1" -> 1, "2" -> 2)
// val m2 = Map("1" -> 0, "2" -> 3)
// GCounter(m1).merge(GCounter(m2))

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemilatice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      import cats.syntax.monoid._
      import cats.instances.list._
      import cats.syntax.foldable._
      import cats.instances.map._

      def increment(f: Map[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K,V] = {
        val value: V = f.getOrElse(k, m.empty) |+| v
        f + (k -> value)
      }

      def merge(f1: Map[K,V], f2: Map[K,V])(implicit b: BoundedSemilatice[V]): Map[K,V] =
        f1 |+| f2


      def total(f: Map[K,V])(implicit m: CommutativeMonoid[V]): V =
        f.values.toList.combineAll
    }
}

val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)

val counter = GCounter[Map, String, Int]
val merged = counter.merge(g1, g2)

import cats.instances.int._
val total = counter.total(merged)

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit val mapInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      def put[K, V](f: Map[K,V])(k: K, v: V): Map[K,V] =
        f + (k -> v)

      def get[K, V](f: Map[K,V])(k: K): Option[V] =
        f.get(k)

      override def getOrElse[K, V](f: Map[K,V])(k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K,V]): List[V] =
        f.values.toList
    }
}

implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
  def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
    kvs.put(f)(key, value)

  def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
    kvs.get(f)(key)

  def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
    kvs.getOrElse(f)(key, default)

  def values(implicit kvs: KeyValueStore[F]): List[V] =
    kvs.values(f)
}

implicit def gcounterInstance[F[_, _], K, V]
    (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
      new GCounter[F, K, V] {
        import cats.syntax.monoid._
        import cats.instances.list._
        import cats.syntax.foldable._

        def increment(f: F[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K,V] = {
          val total = f.getOrElse(k, m.empty) |+| v
          f.put(k, total)
        }

        def merge(f1: F[K,V], f2: F[K,V])(implicit b: BoundedSemilatice[V]): F[K,V] =
          f1 |+| f2

        def total(f: F[K,V])(implicit m: CommutativeMonoid[V]): V =
          f.values.combineAll
      }
