package op.assessment.eslr

import op.assessment.eslr.EuroLottery.{NormalTicket, Ticket}

object Main extends App {

  type WinningClass = NormalTicket => Int

  val inputs = List(
    "1, 2, 3, 4, 5 : 1, 2",
    "3, 4, 5, 6, 7 : 1, 2, 3",
    "3, 4, 5, 6, 7 : 2, 3",
    "6, 7, 8, 9, 10 : 3, 4"
  )

  val solver = new Solver(
    winNumbers = Set(1, 2, 3, 4, 5),
    winStars = Set(1, 2)
  )

  val winningClass: WinningClass = nt => solver(nt.numbers, nt.stars)

  val result = inputs
    .flatMap(Ticket(_))
    .flatMap(_.normalTickets)
    .groupBy(winningClass)
    .toArray.sortBy(_._1)
    .flatMap {
      case (0, _) => None
      case (k, v) => Some(s"Winning class $k: ${v.size}")
    }

  result foreach println

}
