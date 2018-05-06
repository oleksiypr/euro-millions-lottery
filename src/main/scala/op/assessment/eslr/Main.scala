package op.assessment.eslr

import cats.effect.IO
import op.assessment.eslr.EuroLottery.{NormalTicket, Ticket}
import scala.io.Source


/**
  * Program outputs an overview about winnings for the draw in a form
  * Winning class N - number of winning tickets X.
  *
  * Output contains non empty winning classes only, in another words all
  * classes with 0 winners ignored.
  *
  * Usage (assume SBT installed): {{{
  *    sbt "run <path to tickets> <path to winning ticket file>"
  * }}}
  */
object Main extends App {

  type WinningClassification = NormalTicket => Int

  def result(inputs: Seq[String])(w: WinningClassification): Array[String] = {
    inputs
      .flatMap(Ticket(_))
      .flatMap(_.normalTickets)
      .groupBy(w)
      .toArray.sortBy(_._1)
      .flatMap {
        case (0, _) => None
        case (k, v) => Some(s"Winning class $k - ${v.size}")
      }
  }

  def filePathString:  Either[Throwable, (String, String)] = {
    if (args.length < 2) {
      Left(new IllegalArgumentException(
        "Wrong arguments. Usage: <path to tickets> <path to winning ticket file>"))
    } else {
      Right((args(0), args(1)))
    }
  }

  def winningTicket(str: String): Either[Throwable, NormalTicket] = {
    Ticket(str) match {
      case Some(nt: NormalTicket) => Right(nt)
      case _ => Left(new IllegalArgumentException("Winning ticket of wrong format"))
    }
  }

  val program = for {
    paths <- IO.fromEither(filePathString)
    (ticketsPath, winingPath) = paths

    inputs <- IO { Source.fromFile(ticketsPath, "ASCII").getLines() }
    winning <- IO { Source.fromFile(winingPath, "ASCII").getLines().next() }
    wt <- IO.fromEither(winningTicket(winning))

    _ <- IO {
        val solver = new Solver(wt.numbers, wt.stars)
        val w: WinningClassification = nt => solver(nt.numbers, nt.stars)
        result(inputs.toSeq)(w) foreach println
      }
  } yield ()

  program.unsafeRunCancelable {
    case Left(th) => System.err.println(th.getMessage)
    case _ => ()
  }
}
