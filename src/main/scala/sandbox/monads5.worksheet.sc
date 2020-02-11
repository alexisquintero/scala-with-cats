import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

val greeKitty: Reader[Cat, String] =
  catName.map(name => s"Hello $name")

greeKitty.run(Cat("Heathcliff", "junk food"))

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greeKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "lasagne"))
greetAndFeed(Cat("Heathcliff", "junk food"))

// 4.8.3 Exercise: Hacking on Readers

case class Db(
  usernames: Map[Int, String],
  passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(
  username: String,
  password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

def checkLogin(
  userId: Int,
  password: String): DbReader[Boolean] = {
    import cats.syntax.applicative._

    for {
      username <- findUsername(userId)
      passwordOk <- username.map { username =>
                      checkPassword(username, password)
                    }.getOrElse {
                      false.pure[DbReader]
                    }
    } yield passwordOk
}

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)

import cats.data.State

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

val (state, result) = a.run(10).value
val state2 = a.runS(10).value
val result2 = a.runA(10).value

val step1 = State[Int, String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
} yield (a, b)

val (state3, result3) = both.run(20).value

val getDemo = State.get[Int]
getDemo.run(10).value

val setDemo = State.set[Int](30)
setDemo.run(10).value

val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

val inspectDemo = State.inspect[Int, String](_ + "!")
inspectDemo.run(10).value

val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

import State._

val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)

val (state4, result4) = program.run(1).value

// 4.9.3 Exercise: Post-Order Calculator

import cats.data.State

type CalcState[A] = State[List[Int], A]

def operand(num: Int): CalcState[Int] =
  State[List[Int], Int] { stack =>
    (num :: stack, num)
  }

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case b :: a :: tail =>
      val ans: Int = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

def evalOne(sym: String): CalcState[Int] = sym match {
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case num => operand(num.toInt)
}

evalOne("42").run(Nil).value

val program2 = for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program2.runA(Nil).value

def evalAll(input: List[String]): CalcState[Int] = {
  import cats.syntax.applicative._

  input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }
}

val program3 = evalAll(List("1", "2", "+", "3", "*"))

program3.run(Nil).value

val program4 = for {
  _ <- evalAll(List("1", "2", "+"))
  _ <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program4.run(Nil).value

def evalInput(input: String): Int =
  evalAll(input.split(' ').toList).runA(Nil).value

evalInput("1 2 + 3 4 + *")
