package pipegame

/** The interface of the Frame, as presented to the model. */
trait FrameT{
  /** Set the next pieces to be ps. */
  def setNextPieces(ps: List[Piece]): Unit

  /** Set the score to be `score`. */
  //def setScore(score: Int): Unit

  /** Update the info panel. */
  def updateInfo(): Unit

  /** Quit. */
  def quitFrame: Unit

  /** Redraw the main panel. */
  def update(): Unit

  /** Update whether the pipes are currently being filled. */
  def setFilling(f: Boolean): Unit

  /** Reset the scrollbar on the queued pieces. */
  //def resetScrollBar() : Unit
}
