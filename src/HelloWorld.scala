object HelloWorld extends App {
  def printWinner(p: Player): Unit = println(p.name + " is the winner!")

  def winner(p1: Player, p2: Player): Player = {
    if (p1.score > p2.score)
      p1
    else
      p2
  }

  def declareWinner(p1: Player, p2: Player): Unit = printWinner(winner(p1, p2))

  override def main(args: Array[String]) {
    val players = List(Player("Sue", 10),
      Player("Bob", 8),
      Player("Joe", 4))

    val p = players.reduceLeft(winner)

    printWinner(p)
  }
}
