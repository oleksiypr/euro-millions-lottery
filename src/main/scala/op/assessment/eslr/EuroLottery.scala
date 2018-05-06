package op.assessment.eslr

object EuroLottery {

  sealed trait Ticket {
    def numbers: Set[Int]
    def stars: Set[Int]
    def normalTickets: Set[NormalTicket]
  }

  final case class NormalTicket private(
      numbers: Set[Int], stars: Set[Int]
    ) extends Ticket {
    override def normalTickets: Set[NormalTicket] = Set(this)
  }

  final case class SystemTicket private(
      numbers: Set[Int], stars: Set[Int]
    ) extends Ticket {

    override def normalTickets: Set[NormalTicket] = {
      for {
        ns <- combinations(numbers, 5)
        ss <- combinations(stars, 2)
      } yield {
        NormalTicket(ns, ss)
      }
    }
  }

  private def combinations(set: Set[Int], m: Int): Set[Set[Int]] = {
    if (m == 0) Set(Set.empty[Int]) else
    if (set.isEmpty) Set(Set.empty[Int]) else
    if (m >= set.size) Set(set) else {
      for {
        e <- set
        comb <- combinations(set - e, m - 1)
      } yield comb + e
    }
  }

  object Ticket {

    import scala.util.Try
    import cats.instances.option._
    import cats.instances.set._
    import cats.syntax.unorderedTraverse._

    type IntParse = String => Option[Int]

    /**
      * Create [[op.assessment.eslr.EuroLottery.Ticket]] instance based
      * on ticket string iff input is correct.
      * Input consists of list of distinct "numbers" and list of
      * distinct "stars" separated with colon.
      *
      * Correct input format
      * {{{
      *   n1, n2, n3, n4, n5, [optional up to n10] : s1, s2 [optional up to s5]
      * }}}
      *
      * Where {{{
      *   ni - a "number": integer from 1 to 50
      *   si - a "star": integer from 1 to 11
      * }}}
      *
      * Example {{{
      *   1, 2, 3, 4, 5 : 1, 2
      *   3, 4, 5, 6, 7 : 1, 2, 3
      *   1, 2, 3, 4, 5, 6 : 1, 2
      * }}}
      *
      * @param input input string
      * @return [[scala.Some]] of [[op.assessment.eslr.EuroLottery.Ticket]]
      *        if input correct or [[scala.None]] otherwise
      */
    def apply(input: String): Option[Ticket] = {

      def number(str: String): Option[Int] = {
        Try(str.toInt).toOption.filter(n => n >= 1 && n <= 50)
      }

      def star(str: String): Option[Int] = {
        Try(str.toInt).toOption.filter(s => s >= 1 && s <= 11)
      }

      val parts: Option[(String, String)] = {
        val split = input.replaceAll(" ", "").split(":")
        if (split.length == 2) Some((split(0), split(1)))
        else None
      }

      def toInts(str: String)(f: IntParse): Option[Set[Int]] = {
        str.split(",").toSet.unorderedTraverse(f)
      }

      val numbers: Option[Set[Int]] = {
        for {
          (nums, _) <- parts
          n <- toInts(nums)(number)
        } yield n
      }

      val stars: Option[Set[Int]] = {
        for {
          (_, stars) <- parts
          s <- toInts(stars)(star)
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
