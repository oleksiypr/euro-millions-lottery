package op.assessment.eslr

object EuroLottery {

  sealed trait Ticket {
    def numbers: Set[Int]
    def stars: Set[Int]
    def normalTickets: Set[NormalTicket]
  }

  case class NormalTicket private(
      numbers: Set[Int], stars: Set[Int]
    ) extends Ticket {
    override def normalTickets: Set[NormalTicket] = Set(this)
  }

  case class SystemTicket private(
      numbers: Set[Int], stars: Set[Int]
    ) extends Ticket {
    override def normalTickets: Set[NormalTicket] = ???
  }

  object Ticket {

    import scala.util.Try
    import cats.instances.option._
    import cats.instances.set._
    import cats.syntax.unorderedTraverse._

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

      val numbers: Option[Set[Int]] = {
        for {
          (nums, _) <- parts
          n <- nums.split(",").toSet.unorderedTraverse[Option, Int](star)
        } yield n
      }

      val stars: Option[Set[Int]] = {
        for {
          (_, stars) <- parts
          s <- stars.split(",").toSet.unorderedTraverse[Option, Int](star)
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
