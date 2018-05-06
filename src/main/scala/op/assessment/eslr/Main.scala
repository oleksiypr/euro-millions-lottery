package op.assessment.eslr

import cats.effect.IO
import op.assessment.eslr.EuroLottery.{NormalTicket, Ticket}
import scala.io.Source

object Main extends App {

  type WinningClass = NormalTicket => Int

  val solver = new Solver(
    winNumbers = Set(1, 2, 3, 4, 5),
    winStars = Set(1, 2)
  )

  val winningClass: WinningClass = nt => solver(nt.numbers, nt.stars)

  def result(inputs: Seq[String]): Array[String] = inputs
    .flatMap(Ticket(_))
    .flatMap(_.normalTickets)
    .groupBy(winningClass)
    .toArray.sortBy(_._1)
    .flatMap {
      case (0, _) => None
      case (k, v) => Some(s"Winning class $k: ${v.size}")
    }

  if (args.length < 1) {
    System.err.println("Wrong arguments. Usage: <input file>")
    System.exit(1)
  }

  val ticketsFile = args(0)

  val program = for {
    inputs <- IO { Source.fromFile(ticketsFile, "ASCII").getLines() }
    _ <- IO { result(inputs.toSeq) foreach println }
  } yield ()

  program.unsafeRunSync()
}
