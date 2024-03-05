import scala.collection.immutable.BitSet

object Main {
  def main(args: Array[String]): Unit = {
    val puzzle =
      Array(
        Array(1, 0, 0, 0, 0, 6, 5, 0, 0),
        Array(0, 0, 0, 0, 5, 9, 0, 0, 0),
        Array(8, 0, 0, 4, 0, 0, 0, 0, 6),
        Array(2, 0, 0, 7, 0, 0, 0, 0, 4),
        Array(0, 0, 1, 6, 0, 0, 0, 3, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 7, 1, 0, 0, 0, 0, 2),
        Array(0, 0, 9, 0, 0, 0, 0, 0, 5),
        Array(0, 0, 3, 9, 7, 0, 4, 0, 0)
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
  private case class Board(
      grid: Array[Array[Int]],
      rowContains: Array[BitSet],
      colContains: Array[BitSet],
      cellContains: Array[BitSet],
      emptySquares: List[(Int, Int)]
  ) {
    def updated(r: Int, c: Int, move: Int): Board = {
      val newGrid = grid.updated(r, grid(r).updated(c, move))
      val newRowContains = rowContains.updated(r, rowContains(r) + move)
      val newColContains = colContains.updated(c, colContains(c) + move)
      val cell = (r / 3) * 3 + c / 3
      val newCellContains =
        cellContains.updated(cell, cellContains(cell) + move)
      Board(
        newGrid,
        newRowContains,
        newColContains,
        newCellContains,
        emptySquares.tail
      )
    }

    def validMoves(r: Int, c: Int): Set[Int] = {
      val allMoves = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val cell = (r / 3) * 3 + c / 3
      allMoves -- rowContains(r) -- colContains(c) -- cellContains(cell)
    }
  }
  def solve(grid: Array[Array[Int]]): Option[Array[Array[Int]]] = {
    val rowContains = Range(0, 9).map(r => BitSet(grid(r): _*)).toArray
    val colContains =
      Range(0, 9).map(c => BitSet(grid.map(row => row(c)): _*)).toArray
    val cellContains = Range(0, 9).map { cell =>
      val r = cell / 3 * 3
      val c = cell % 3 * 3
      BitSet(grid.slice(r, r + 3).flatMap(row => row.slice(c, c + 3)): _*)
    }.toArray

    val empty = for {
      r <- 0 until 9
      c <- 0 until 9
      if grid(r)(c) == 0
    } yield (r, c)

    val board =
      Board(grid, rowContains, colContains, cellContains, empty.toList)

    def helper(board: Board): Option[Board] =
      board.emptySquares match {
        case Nil => Some(board)
        case (r, c) :: _ =>
          board
            .validMoves(r, c)
            .view
            .flatMap(move => helper(board.updated(r, c, move)))
            .headOption
      }

    helper(board).map(_.grid)
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
