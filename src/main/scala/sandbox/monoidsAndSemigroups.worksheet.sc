// trait Monoid[A] {
//   def combine(x: A, y: A): A
//   def empty: A
// }

def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
  m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
}

def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
  (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}

def testLaws[A](ls: List[A])(implicit m: Monoid[A]): Boolean = {
  val assoc: List[Boolean] = for {
    x <- ls
    y <- ls
    z <- ls
  } yield associativeLaw(x, y, z)(m)

  val iden: List[Boolean] = for {
    x <- ls
  } yield identityLaw(x)(m)

  (assoc ++ iden).reduce(_ && _)
}

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

// 2.3 Exercise: The Truth About Monoids

implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
  def combine(x: Boolean, y: Boolean): Boolean = x || y
  def empty: Boolean = false
}

testLaws(List(true, false))(booleanOrMonoid)

implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
  def combine(x: Boolean, y: Boolean): Boolean = x && y
  def empty: Boolean = true
}

testLaws(List(true, false))(booleanAndMonoid)

implicit val booleanMIMonoid: Monoid[Boolean] = new Monoid[Boolean] {
  def combine(x: Boolean, y: Boolean): Boolean = if (x) y else !y
  def empty: Boolean = true
}

testLaws(List(true, false))(booleanMIMonoid)

implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
  def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  def empty: Boolean = false
}

testLaws(List(true, false))(booleanEitherMonoid)

implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
  def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  def empty: Boolean = true
}

testLaws(List(true, false))(booleanXnorMonoid)

// 2.4 Exercise: All Set for Monoids

implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
  def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  def empty: Set[A] = Set.empty[A]
}

testLaws(List(Set(1, 2), Set(2, 3)))(setUnionMonoid)

// val intSetMonoid = Monoid[Set[Int]]
// val strSetMonoid = Monoid[Set[String]]

// intSetMonoid.combine(Set(1, 2), Set(2, 3))
// strSetMonoid.combine(Set("A", "B"), Set("B", "C"))

implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
  def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
}

implicit def symDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
  def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  def empty: Set[A] = Set.empty
}

testLaws(List(Set(1, 2), Set(2, 3)))(symDiffMonoid)
