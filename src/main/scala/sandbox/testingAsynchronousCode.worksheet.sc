import scala.concurrent.Future

// trait UptimeClient {
//   def getUptime(hostname: String): Future[Int]
// }

// import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
// import scala.concurrent.ExecutionContext.Implicits.global

// val hostnames = List(
//   "alpha.example.com",
//   "beta.example.com",
//   "gamma.demo.com"
// )

// class UptimeService(client: UptimeClient) {
//   def getTotalUptime(hostnames: List[String]): Future[Int] =
//     hostnames.traverse(client.getUptime).map(_.sum)
// }

// class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
//   def getUptime(hostname: String): Future[Int] =
//     Future.successful(hosts.getOrElse(hostname, 0))
// }

// def testTotalUptime() = {
//   val hosts = Map("host1" -> 10, "host2" -> 6)
//   val client = new TestUptimeClient(hosts)
//   val service = new UptimeService(client)
//   val actual = service.getTotalUptime(hosts.keys.toList)
//   val expected = hosts.values.sum
//   assert(actual == expected)
// }

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

import cats.Id

// trait TestUptimeClient extends UptimeClient[Id] {
//   def getUptime(hostname: String): Int
// }

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

import cats.Applicative
import cats.syntax.functor._

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

def testTotalUptime() = {
  val hosts = Map("host1" -> 10, "host2" -> 6)
  val client = new TestUptimeClient(hosts)
  val service = new UptimeService(client)
  val actual = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  // assert(actual == expected)
  actual == expected
}

testTotalUptime()
