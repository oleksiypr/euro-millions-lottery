package op.assessment.eslr

import scala.util.Try
import cats.instances.option._
import cats.instances.list._
import cats.syntax.traverse._

object EuroLottery {

  trait Ticket {
    def numbers: List[Int]
    def stars: List[Int]
    def normalTickets: List[NormalTicket]
  }

  case class NormalTicket(numbers: List[Int], stars: List[Int]) extends Ticket {
    override def normalTickets: List[NormalTicket] = List(this)
  }

  case class SystemTicket(numbers: List[Int], stars: List[Int]) extends Ticket {
    override def normalTickets: List[NormalTicket] = ???
  }

  object Ticket {

    def apply(input: String): Option[Ticket] = {
      def number(s: String): Option[Int] =
        Try(s.toInt).toOption.filter(n => n >= 1 && n <= 50)

      def star(s: String): Option[Int] =
        Try(s.toInt).toOption.filter(s => s >= 1 && s<= 3)

      val parts = input.replaceAll(" ", "").split(":")

      val numbers: Option[List[Int]] = {
        val list = parts(0).split(",").toList
        list.traverse[Option, Int](number)
      }

      val stars: Option[List[Int]] = {
        val list = parts(1).split(",").toList
        list.traverse[Option, Int](star)
      }

      for {
        ns <- numbers if ns.size == 5
        ss <- stars if ss.size == 2
      } yield NormalTicket(ns, ss)
    }
  }
}
