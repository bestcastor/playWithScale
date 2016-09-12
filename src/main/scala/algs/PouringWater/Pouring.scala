package algs.PouringWater

/**
  * Created by nyu on 9/11/16.
  */
class Pouring(capacity : Vector[Int]) {
  //States
  type State = Vector[Int]

  //Initial state
  val initialState = capacity map (x => 0)

  //Move
  trait Move {
    def change(state:State):State
  }
  case class Empty(glass:Int) extends Move {
//    def change(state:State):State = {
//      state(glass) = 0
//      state
//    }
    def change(state:State) = state updated (glass, 0)
  }
  case class Fill(glass:Int) extends Move {
//    def change(state:State):State = {
//      state(glass) = capacity(glass)
//      state
//    }
    def change(state:State) = state updated (glass, capacity(glass))
  }
  case class Pour(from:Int, to:Int) extends Move {

    def change(state:State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.size
  //All the possible moves
  val moves =
    (for(g <- glasses) yield Empty(g)) ++
      (for(g <- glasses) yield Fill(g)) ++
      (for{from <- glasses; to <- glasses if (from != to)} yield Pour(from, to))

  //Path
  //**** To even improve the performance, we can remember the history endState as well.
  // Note: We need to define it as 'val', so that the other functions can access this parameter
  class Path(histories: List[Move], val endState: State) {
//    //Way-1
//    val endState1: State = traceState(histories)
//
//    private def traceState(xs: List[Move]): State = xs match {
//      case Nil => initialState
//      case move :: xs1 => move.change(traceState(xs1))
//    }
//    //Way-2
//    val endState: State = (histories foldRight initialState) (_ change _)

    def extend(move:Move) = new Path(move::histories, move change endState)

    override def toString = (histories.reverse mkString(" ") ) + " --> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths:Set[Path], explored: Set[State]) : Stream[Set[Path]]= {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if (! (explored contains next.endState))
      } yield next

      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  val pathSet = from(Set(initialPath), Set(initialState))

  def solutions(target:Int) : Stream[Path] =
    for {
      pathSet <- pathSet
      path <- pathSet
      if path.endState contains target
    } yield path

}
