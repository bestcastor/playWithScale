package algs

/**
  * Created by nyu on 8/27/16.
  */
object nQueens {

  def queens(n:Int):Set[List[Int]] = {
    def placeQueen(row:Int):Set[List[Int]]=
      if (row == 0) Set(List())
      else
        for {
          queens <- placeQueen(row -1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens

    placeQueen(n)
  }

  def isSafe(col:Int, queens:List[Int]):Boolean = {
    var row = queens.length
//    val pairL = queens map (i => {
//      row = row-1
//      (row, i)})
    val queensWithPair = (row-1 to 0 by -1) zip queens
    queensWithPair forall {
      case (r, c) => col != c && Math.abs(col - c) != row -r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

}