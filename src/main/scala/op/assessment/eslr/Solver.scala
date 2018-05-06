package op.assessment.eslr

/**
  * Check numbers and stars combination against winning combinations in order
  * to resolve winning class.
  *
  * @param winNumbers winning numbers as a set of 5 distinct integers
  *                   from 1 to 50 each
  * @param winStars winning stars as a set of 2 distinct integers 1 to 11 each
  */
class Solver(winNumbers: Set[Int], winStars: Set[Int]) {

  /**
    * Resolve winning class as integer number from 1 to 13.
    * If a ticket does not correspond to any wining class result will be 0.
    *
    * @param numbers set of 5 distinct integers from 1 to 50 each
    * @param stars set of 2 distinct integers from 1 to 11 each
    * @return winning class as integer number
    */
  def apply(numbers: Set[Int], stars: Set[Int]): Int = {
    val ns = (winNumbers intersect numbers).size
    val ss = (winStars intersect stars).size

    (ns, ss) match {
      case (5, 2) =>  1
      case (5, 1) =>  2
      case (5, 0) =>  3
      case (4, 2) =>  4
      case (4, 1) =>  5
      case (4, 0) =>  6
      case (3, 2) =>  7
      case (2, 2) =>  8
      case (3, 1) =>  9
      case (3, 0) => 10
      case (1, 2) => 11
      case (2, 1) => 12
      case (2, 0) => 13
      case (_, _) =>  0
    }
  }
}
