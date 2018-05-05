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
      def number(str: String): Option[Int] =
        Try(str.toInt).toOption.filter(n => n >= 1 && n <= 50)

      def star(str: String): Option[Int] =
        Try(str.toInt).toOption.filter(s => s >= 1 && s<= 3)

      val parts: Option[(String, String)] = {
        val splitted = input.replaceAll(" ", "").split(":")
        if (splitted.length == 2) Some((splitted(0), splitted(1)))
        else None
      }

      def numbers: Option[List[Int]] = {
        for {
          (nums, _) <- parts
          n <- nums.split(",").toList.traverse[Option, Int](number)
        } yield n
      }

      def stars: Option[List[Int]] = {
        for {
          (_, stars) <- parts
          s <- stars.split(",").toList.traverse[Option, Int](number)
        } yield s
      }

      for {
        ns <- numbers if ns.size == 5
        ss <- stars if ss.size == 2
      } yield NormalTicket(ns, ss)
    }
  }
}
