package pipegame

import BasePanel._

import scala.swing._

/** The top panel in the game.  Displays the queue of the next `width` pieces. */
class TopPanel(model: Model) extends BasePanel(model.NumNextPieces+1, 1){
  /** The next pieces. */
  private var nextPieces = List[Piece]()

  /** Set the next peices to be ps, and repaint. */
  def setNextPieces(ps: List[Piece]) = { 
    assert(ps.length == width-1, ps); nextPieces = ps; repaint() 
  }

  /** Paint this component. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setColor(BackgroundColour)
    g.fillRect(Pad, Pad, scale(width), scale(2))

    // Paint nextPieces
    val offSet = Pad 
    g.setColor(CurrentPipeColour)
    drawPiece(g, Pad, Pad+SquareSize, model.getCurrentPiece) //  nextPieces(0))
    g.setColor(QueuedPipeColour)
    for(i <- 0 until nextPieces.length){
      drawPiece(g, Pad+scale(i+1), Pad+SquareSize, nextPieces(i))
    }
  }
}
