import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val puzzle =
      Array(
        Array(5, 3, 0, 0, 7, 0, 0, 0, 0),
        Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
        Array(0, 9, 8, 0, 0, 0, 0, 6, 0),
        Array(8, 0, 0, 0, 6, 0, 0, 0, 3),
        Array(4, 0, 0, 8, 0, 3, 0, 0, 1),
        Array(7, 0, 0, 0, 2, 0, 0, 0, 6),
        Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
        Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
        Array(0, 0, 0, 0, 8, 0, 0, 7, 9)
      )
    // Warmup iteration, cuts time down by 100x
    Sudoku.solve(puzzle)
    println(s"Initial puzzle: ${Sudoku.render(puzzle)}")

    val startTime = System.nanoTime()
    val solution = Sudoku.solve(puzzle)
    val solveTime = (System.nanoTime() - startTime) / 1e6
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
      rowContains: IndexedSeq[mutable.BitSet],
      colContains: IndexedSeq[mutable.BitSet],
      cellContains: IndexedSeq[mutable.BitSet],
      var emptySquares: List[(Int, Int)]
  ) {
    private val allMoves = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
    private def getCell(r: Int, c: Int): Int = (r / 3) * 3 + c / 3
    def move(r: Int, c: Int, move: Int): Unit = {
      grid(r)(c) = move
      rowContains(r) += move
      colContains(c) += move
      val cell = getCell(r, c)
      cellContains(cell) += move
      emptySquares = emptySquares.tail
    }

    def undoMove(r: Int, c: Int, move: Int): Unit = {
      rowContains(r) -= move
      colContains(c) -= move
      val cell = getCell(r, c)
      cellContains(cell) -= move
      emptySquares = (r, c) +: emptySquares
    }

    def validMoves(r: Int, c: Int): Set[Int] = {
      val cell = getCell(r, c)
      allMoves -- rowContains(r) -- colContains(c) -- cellContains(cell)
    }
  }
  def solve(grid: Array[Array[Int]]): Option[Array[Array[Int]]] = {
    val rowContains = Range(0, 9).map(r => mutable.BitSet(grid(r): _*))
    val colContains =
      Range(0, 9).map(c => mutable.BitSet(grid.map(row => row(c)): _*))
    val cellContains = Range(0, 9).map { cell =>
      val r = cell / 3 * 3
      val c = cell % 3 * 3
      mutable.BitSet(
        grid.slice(r, r + 3).flatMap(row => row.slice(c, c + 3)): _*
      )
    }

    val empty = for {
      r <- 0 until 9
      c <- 0 until 9
      if grid(r)(c) == 0
    } yield (r, c)

    val board =
      Board(grid, rowContains, colContains, cellContains, empty.toList)

    def helper(board: Board): Boolean = {
      board.emptySquares match {
        case Nil => true
        case (r, c) :: _ =>
          val moves = board.validMoves(r, c)
          moves.exists { move =>
            board.move(r, c, move)
            val result = helper(board)
            board.undoMove(r, c, move)
            result
          }
      }
    }

    val result = helper(board)
    if (result) Some(board.grid) else None
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
