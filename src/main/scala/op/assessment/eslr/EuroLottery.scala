package op.assessment.eslr

object EuroLottery {

  sealed trait Ticket {
    def numbers: List[Int]
    def stars: List[Int]
    def normalTickets: List[NormalTicket]
  }

  case class NormalTicket private(
      numbers: List[Int], stars: List[Int]
    ) extends Ticket {
    override def normalTickets: List[NormalTicket] = List(this)
  }

  case class SystemTicket private(
      numbers: List[Int], stars: List[Int]
    ) extends Ticket {
    override def normalTickets: List[NormalTicket] = ???
  }

  object Ticket {

    import scala.util.Try
    import cats.instances.option._
    import cats.instances.list._
    import cats.syntax.traverse._

    def apply(input: String): Option[Ticket] = {

      def number(str: String): Option[Int] =
        Try(str.toInt).toOption.filter(n => n >= 1 && n <= 50)

      def star(str: String): Option[Int] =
        Try(str.toInt).toOption.filter(s => s >= 1 && s <= 11)

      val parts: Option[(String, String)] = {
        val split = input.replaceAll(" ", "").split(":")
        if (split.length == 2) Some((split(0), split(1)))
        else None
      }

      val numbers: Option[List[Int]] = {
        for {
          (nums, _) <- parts
          n <- nums.split(",").toList.traverse[Option, Int](number)
        } yield n
      }

      val stars: Option[List[Int]] = {
        for {
          (_, stars) <- parts
          s <- stars.split(",").toList.traverse[Option, Int](star)
        } yield s
      }

      val normal = for {
        ns <- numbers if ns.size == 5
        ss <- stars if ss.size == 2
      } yield {
        NormalTicket(ns, ss)
      }

      def system = for {
        ns <- numbers if ns.size >= 5 && ns.size <= 10
        ss <- stars if ss.size >= 2 && ss.size <= 5
      } yield {
        SystemTicket(ns, ss)
      }

      normal orElse system
    }
  }
}
