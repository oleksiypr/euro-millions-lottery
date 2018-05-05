package op.assessment.eslr

import op.assessment.eslr.EuroLottery.{NormalTicket, SystemTicket, Ticket}
import org.scalatest.FunSuite

class EuroLotteryTest extends FunSuite {

  test("data structure") {
    Ticket("1, 2, 3, 4, 5 : 1, 2") match {
      case Some(ticket) =>
        assert(ticket == NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)))
      case None => fail("Some ticket expected but actual none")
    }

    Ticket("1, 2, 3, 4, 5, 6 : 1, 2") match {
      case Some(ticket) =>
        assert(ticket == SystemTicket(Set(1, 2, 3, 4, 5, 6), Set(1, 2)))
      case None => fail("Some ticket expected but actual none")
    }

    Ticket("1, 2, 3, 4, 5 : 1, 2, 3") match {
      case Some(ticket) =>
        assert(ticket == SystemTicket(Set(1, 2, 3, 4, 5), Set(1, 2, 3)))
      case None => fail("Some ticket expected but actual none")
    }

    Ticket("1, 2, 3, 4, 5, 6, 7, 8, 9, 10 : 1, 2") match {
      case Some(ticket) =>
        assert(ticket == SystemTicket(
          Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Set(1, 2)))
      case None => fail("Some ticket expected but actual none")
    }

    Ticket("1, 2, 3, 4, 5 : 1, 2, 3, 4, 5") match {
      case Some(ticket) =>
        assert(ticket == SystemTicket(
          Set(1, 2, 3, 4, 5), Set(1, 2, 3, 4, 5)))
      case None => fail("Some ticket expected but actual none")
    }

    Ticket("1, 2, 3, 4, 5, 6 : 1, 2, 3") match {
      case Some(ticket) =>
        assert(ticket == SystemTicket(
          Set(1, 2, 3, 4, 5, 6), Set(1, 2, 3)))
      case None => fail("Some ticket expected but actual none")
    }
  }

  test("data structure: empty numbers") {
    assert(Ticket(": 1, 2") === None)
  }

  test("data structure: empty stars") {
    assert(Ticket("1, 2, 3, 4, 5 :") === None)
    assert(Ticket("1, 2, 3, 4, 5") === None)
  }

  test("data structure: incorrect input") {
    assert(Ticket("") === None)
    assert(Ticket("1, 2, 3, as, 5 : 1, 2") === None)
    assert(Ticket("sdfdsfsd") === None)
    assert(Ticket("1, 2, 3, 51, 5 : 1, 2") === None)
    assert(Ticket("1, 2, 3, 4, 5 : 1, 12") === None)
    assert(Ticket("1, 0, 3, 4, 5 : 1, 6") === None)
    assert(Ticket("-1, 2, 3, 4, 5 : 1, 6") === None)
    assert(Ticket("1, 2, 3, 4, 5 : 0, 11") === None)
  }

  test("all normal tickets") {
    val tickets: Set[Ticket] = Set()
    val normals: Set[NormalTicket] = tickets.flatMap(_.normalTickets)
  }
}
