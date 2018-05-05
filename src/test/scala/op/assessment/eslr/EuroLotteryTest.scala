package op.assessment.eslr

import op.assessment.eslr.EuroLottery.{NormalTicket, SystemTicket, Ticket}
import org.scalatest.FunSuite

class EuroLotteryTest extends FunSuite {

  test("data structure") {
    Ticket("1, 2, 3, 4, 5 : 1, 2") match {
      case Some(NormalTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5))
        assert(ss === List(1, 2))
      case x => assert(x === None)
    }

    Ticket("1, 2, 3, 4, 5, 6 : 1, 2") match {
      case Some(SystemTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5, 6))
        assert(ss === List(1, 2))
      case x => assert(x === None)
    }

    Ticket("1, 2, 3, 4, 5 : 1, 2, 3") match {
      case Some(SystemTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5))
        assert(ss === List(1, 2, 3))
      case x => assert(x === None)
    }

    Ticket("1, 2, 3, 4, 5, 6, 7, 8, 9, 10 : 1, 2") match {
      case Some(SystemTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        assert(ss === List(1, 2))
      case x => assert(x === None)
    }

    Ticket("1, 2, 3, 4, 5 : 1, 2, 3, 4, 5") match {
      case Some(SystemTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5))
        assert(ss === List(1, 2, 3, 4, 5))
      case x => assert(x === None)
    }

    Ticket("1, 2, 3, 4, 5, 6 : 1, 2, 3") match {
      case Some(SystemTicket(ns, ss)) =>
        assert(ns === List(1, 2, 3, 4, 5, 6))
        assert(ss === List(1, 2, 3))
      case x => assert(x === None)
    }
  }

  test("data structure: empty numbers") {
    Ticket(": 1, 2") match {
      case None => succeed
      case x => assert(x === None)
    }
  }

  test("data structure: empty stars") {
    Ticket("1, 2, 3, 4, 5 :") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 2, 3, 4, 5") match {
      case None => succeed
      case x => assert(x === None)
    }
  }

  test("data structure: incorrect input") {
    Ticket("") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 2, 3, as, 5 : 1, 2") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("sdfdsfsd") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 2, 3, 51, 5 : 1, 2") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 2, 3, 4, 5 : 1, 12") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 0, 3, 4, 5 : 1, 6") match {
      case None => succeed
      case x => assert(x === None)
    }
    Ticket("1, 2, 3, 4, 5 : 0, 11") match {
      case None => succeed
      case x => assert(x === None)
    }
  }

  test("all normal tickets") {
    val tickets: List[Ticket] = List()
    val normals: List[NormalTicket] = tickets.flatMap(_.normalTickets)
  }
}
