import scala.collection.immutable.Nil
val x = {
  println("Computing x")
  math.random
}
x
x

def y = {
  println("Computing Y")
  math.random
}
y
y

lazy val z = {
  println("Computing Z")
  math.random
}
z
z

import cats.Eval

val now = Eval.now(math.random + 1000)
val later = Eval.later(math.random + 2000)
val always = Eval.always(math.random + 3000)

now.value
now.value
later.value
later.value
always.value
always.value

val x2 = Eval.now {
  println("Computing X2")
  math.random
}
x2.value
x2.value

val y2 = Eval.always {
  println("Computing Y2")
  math.random
}
y2.value
y2.value

val z2 = Eval.later {
  println("Computing Z2")
  math.random
}
z2.value
z2.value

val greeting = Eval.
  always { println("Step 1"); "Hello" }.
  map { str => println("Step 2"); s"$str world" }

greeting.value

val ans = for {
  a <- Eval.now { println("Calculating A"); 40 }
  b <- Eval.always { println("Calculating B"); 2 }
} yield {
  println("Adding A and B")
  a + b
}

ans.value
ans.value

val saying = Eval.
  always { println("Step 1"); "The cat" }.
  map { str => println("Step 2"); s"$str sat on" }.
  memoize.
  map { str => println("Step 3"); s"$str the mat" }

saying.value
saying.value

def factorial(n: BigInt): BigInt =
  if(n == 1) n else n * factorial(n - 1)

//factorial(50000) //Stack overflow

def factorial2(n: BigInt): Eval[BigInt] =
  if(n == 1) {
    Eval.now(n)
  } else {
    factorial2(n - 1).map(_ * n)
  }

// factorial2(50000).value //Stack overflow

def factorial3(n: BigInt): Eval[BigInt] =
  if(n == 1) {
    Eval.now(n)
  } else {
    Eval.defer(factorial3(n - 1).map(_ * n))
  }

factorial3(50000).value

// 4.6.5 Exercise: Safer Folding using Eval

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  as match {
    case head :: tail =>
      fn(head, foldRight(tail, acc)(fn))
    case Nil =>
      acc
  }

// def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
//   case head :: tail =>
//     Eval.now(fn(head, foldRight2(tail, acc)(fn).value))
//   case Nil =>
//     Eval.now(acc)
// }

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
  case head :: tail =>
    Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
  case Nil =>
    acc
}

def foldRight3[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc)) { (a, b) =>
    b.map(fn(a, _))
  }.value

// foldRight2((1 to 100000).toList, 0L)(_ + _)
foldRight3((1 to 100000).toList, 0L)(_ + _)
