object Main {
  def main(args: Array[String]): Unit = {
    val puzzle =
      Array(
        Array(0, 0, 8, 0, 0, 7, 0, 0, 0),
        Array(0, 4, 0, 9, 0, 0, 0, 6, 0),
        Array(1, 9, 0, 0, 8, 3, 5, 0, 0),
        Array(0, 0, 0, 0, 0, 8, 0, 1, 0),
        Array(6, 0, 0, 0, 0, 0, 0, 4, 0),
        Array(0, 0, 0, 7, 0, 0, 3, 0, 0),
        Array(8, 0, 1, 0, 0, 9, 2, 7, 0),
        Array(0, 0, 3, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 6, 0, 0, 0, 4, 0, 5)
      )

    println(s"Initial puzzle: ${Sudoku.render(puzzle)}")

    val startTime = System.currentTimeMillis()
    val solution = Sudoku.solve(puzzle)
    val solveTime = System.currentTimeMillis() - startTime
    solution match {
      case Some(s) =>
        println(s"Solution found in ${solveTime}ms! ${Sudoku.render(s)}")
      case None => println("No solution found")
    }
  }
}

object Sudoku {
  def solve(
      grid: Array[Array[Int]],
      startRow: Int = 0,
      startCol: Int = 0
  ): Option[Array[Array[Int]]] =
    findFirstEmpty(grid, startRow, startCol) match {
      case None => Some(grid)
      case Some((r, c)) =>
        val nextRow = r + (c + 1) / 9
        val nextCol = (c + 1) % 9
        val newGrids = validMoves(grid, r, c).map(move =>
          grid.updated(r, grid(r).updated(c, move))
        )
        newGrids.map(grid => solve(grid, nextRow, nextCol)).collectFirst {
          case Some(grid) => grid
        }
    }

  private def findFirstEmpty(
      grid: Array[Array[Int]],
      startRow: Int,
      startCol: Int
  ): Option[(Int, Int)] = {
    for (r <- startRow until 9) {
      val colStart = if (r == 0) startCol else 0
      for (c <- colStart until 9) {
        if (grid(r)(c) == 0) return Some((r, c))
      }
    }
    None
  }

  private def validMoves(grid: Array[Array[Int]], r: Int, c: Int): Seq[Int] =
    grid(r)(c) match {
      case 0 =>
        val row = grid(r)
        val col = grid.map(row => row(c))
        val cell = grid
          .slice((r / 3) * 3, (r / 3) * 3 + 3)
          .flatMap(row => row.slice((c / 3) * 3, (c / 3) * 3 + 3))

        for (
          m <- 1 to 9
          if !row.contains(m) && !col.contains(m) && !cell.contains(m)
        ) yield m
      case _ => Seq.empty
    }

  private def isValid(grid: Array[Array[Int]]): Boolean = !Range(0, 9).exists {
    i =>
      val row = Range(0, 9).map(grid(i)(_)).filter(_ != 0)
      val col = Range(0, 9).map(grid(_)(i)).filter(_ != 0)
      val square = Range(0, 9)
        .map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
        .filter(_ != 0)
      row.distinct.length != row.length ||
      col.distinct.length != col.length ||
      square.distinct.length != square.length
  }

  def render(grid: Array[Array[Int]]): String = {
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
