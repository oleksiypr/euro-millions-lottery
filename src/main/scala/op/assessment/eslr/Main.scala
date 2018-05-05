package op.assessment.eslr

object Main extends App {

  def combinations(set: Set[Int], m: Int): Set[Set[Int]] = {
    if (m == 0) Set(Set.empty[Int]) else
    if (set.isEmpty) Set(Set.empty[Int]) else
    if (m >= set.size) Set(set) else {
      for {
        e <- set
        comb <- combinations(set - e, m - 1)
      } yield comb + e
    }
  }

  println(combinations(Set(1, 2, 3, 4, 5), 3))
}
