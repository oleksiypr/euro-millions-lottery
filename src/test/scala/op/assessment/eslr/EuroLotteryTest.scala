package op.assessment.eslr

import op.assessment.eslr.EuroLottery.{NormalTicket, Ticket}
import org.scalatest.FunSuite

class EuroLotteryTest extends FunSuite {

  test("data structure") {

    Ticket("1, 2, 3, 4, 5 : 1, 2") match {
      case Some(NormalTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5))
        assert(ss === List(1, 2))
      case x => fail()
    }
  }

  test("all normal tickets") {
    val tickets: List[Ticket] = List()
    val normals: List[NormalTicket] = tickets.flatMap(_.normalTickets)
  }
}
