package pipegame

/** The interface of the Frame, as presented to the model. */
trait FrameT{
  /** Set the next pieces to be ps. */
  def setNextPieces(ps: List[Piece]): Unit

  /** Set the score to be `score`. */
  def setScore(score: Int): Unit

  def quitFrame: Unit

}
