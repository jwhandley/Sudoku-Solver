import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    val puzzle = Sudoku(
      Vector(
        Vector(1, 0, 0, 0, 0, 6, 5, 0, 0),
        Vector(0, 0, 0, 0, 5, 9, 0, 0, 0),
        Vector(8, 0, 0, 4, 0, 0, 0, 0, 6),
        Vector(2, 0, 0, 7, 0, 0, 0, 0, 4),
        Vector(0, 0, 1, 6, 0, 0, 0, 3, 0),
        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
        Vector(0, 0, 7, 1, 0, 0, 0, 0, 2),
        Vector(0, 0, 9, 0, 0, 0, 0, 0, 5),
        Vector(0, 0, 3, 9, 7, 0, 4, 0, 0)
      )
    )

    val startTime = System.currentTimeMillis()
    val solution = puzzle.solve()
    solution match {
      case Some(s) =>
        println(
          s"Solution found in ${System.currentTimeMillis() - startTime}ms! ${s.render()}"
        )
      case None => println("No solution found")
    }

//    puzzle.validateState()
  }
}

object Sudoku {
  def parseInput(input: String): Sudoku = {
    if (input.length != 81)
      throw new Exception("Sudoku board must be 81 characters long")
    Sudoku(
      input
        .grouped(9)
        .map(row =>
          row.map {
            case c if c.isDigit => c.asDigit
            case c if c == '.'  => 0
            case _ =>
              throw new Exception(
                "Sudoku board must contain only digits or '.'s"
              )
          }.toVector
        )
        .toVector
    )
  }
}

case class Sudoku(grid: Vector[Vector[Int]]) {
  def solve(): Option[Sudoku] = {
    @tailrec
    def rec(toSolve: List[Sudoku], solved: List[Sudoku] = Nil): List[Sudoku] = {
      toSolve match {
        case Nil => solved
        case head :: tail =>
          val grid = head.grid
          val zeros = for {
            r <- 0 until 9
            c <- 0 until 9 if grid(r)(c) == 0
          } yield (r, c)

          zeros.headOption match {
            case None =>
              rec(tail, head +: solved)
            case Some((r, c)) =>
              val newGrids = head
                .validMoves(r, c)
                .map(move => head.updated(r, c, move))
                .toList
              rec(newGrids ++ tail, solved)
          }
      }
    }
    if (!isValid) None
    else rec(List(this)).headOption
  }

  private def updated(r: Int, c: Int, move: Int): Sudoku = Sudoku(
    grid.updated(r, grid(r).updated(c, move))
  )
  private def validMoves(r: Int, c: Int): Seq[Int] = grid(r)(c) match {
    case 0 =>
      val row = grid(r)
      val col = grid.map(row => row(c))
      val cell = grid
        .slice((r / 3) * 3, (r / 3) * 3 + 3)
        .flatMap(row => row.slice((c / 3) * 3, (c / 3) * 3 + 3))

      for (
        m <- 1 to 9 if !row.contains(m) && !col.contains(m) && !cell.contains(m)
      ) yield m
    case _ => Seq.empty
  }

  private def isValid: Boolean = !Range(0, 9).exists { i =>
    val row = Range(0, 9).map(grid(i)(_)).filter(_ != 0)
    val col = Range(0, 9).map(grid(_)(i)).filter(_ != 0)
    val square = Range(0, 9)
      .map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
      .filter(_ != 0)
    row.distinct.length != row.length ||
    col.distinct.length != col.length ||
    square.distinct.length != square.length
  }

  def render(): String = {
    val rowSeparator = "\n+-------+-------+-------+\n"
    grid
      .map(row =>
        row
          .map(i => if (i == 0) " " else i.toString)
          .grouped(3)
          .map(_.mkString(" "))
          .mkString("| ", " | ", " |")
      )
      .grouped(3)
      .map(_.mkString("\n"))
      .mkString(rowSeparator, rowSeparator, rowSeparator)
  }
}
