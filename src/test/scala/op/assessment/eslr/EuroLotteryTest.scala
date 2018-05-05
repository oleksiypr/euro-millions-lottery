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

    Ticket("1, 2, 3, 4, 40 : 1, 2") match {
      case Some(ticket) =>
        assert(ticket == NormalTicket(Set(1, 2, 3, 4, 40), Set(1, 2)))
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

  test("no tickets") {
    assert(List.empty[Ticket].flatMap(_.normalTickets) === List.empty[Ticket])
  }

  test("all normal tickets") {
    val one = List(NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)))
    assert(one.flatMap(_.normalTickets) ===
      List(NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2))))

    val many = List(
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)),
      NormalTicket(Set(3, 7, 1, 9, 4), Set(5, 9)),
      NormalTicket(Set(3, 7, 1, 9, 4), Set(5, 9))
    )

    assert(many.flatMap(_.normalTickets) === List(
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)),
      NormalTicket(Set(3, 7, 1, 9, 4), Set(5, 9)),
      NormalTicket(Set(3, 7, 1, 9, 4), Set(5, 9))
    ))

    val same = List(
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)),
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2))
    )
    assert(same.flatMap(_.normalTickets).size == 2)
  }

  test("system ticket") {
    val one = List(SystemTicket(Set(1, 2, 3, 4, 5), Set(1, 2, 3)))
    val tickets: List[NormalTicket] = one.flatMap(_.normalTickets)
    assert(tickets === List(
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 2)),
      NormalTicket(Set(1, 2, 3, 4, 5), Set(1, 3)),
      NormalTicket(Set(1, 2, 3, 4, 5), Set(2, 3))
    ))

    val same  = List(
      SystemTicket(Set(1, 2, 3, 4, 5), Set(1, 2, 3)),
      SystemTicket(Set(1, 2, 3, 4, 5), Set(1, 2, 3))
    )

    assert(same.flatMap(_.normalTickets).size == 6)
  }
}
