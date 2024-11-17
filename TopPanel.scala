package pipegame

import BasePanel._

import scala.swing._

class TopPanel(model: Model) extends BasePanel{
  /** The next pieces. */
  private var nextPieces = List[Piece]()

  /** Number of next pieces. */
  private var n = 0

  /** Set the next peices to be ps, and repaint. */
  def setNextPieces(ps: List[Piece]) = {
    nextPieces = ps; n = ps.length; setSize(n,1); repaint()
  }

  /** Paint this component. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setColor(BackgroundColour)
    g.fillRect(Pad, Pad, scale(n), scale(2))

    // Paint nextPieces
    val offSet = Pad
    g.setColor(CurrentPipeColour)
    drawPiece(g, Pad, Pad+SquareSize, model.getCurrentPiece) //  nextPieces(0))
    g.setColor(QueuedPipeColour)
    for(i <- 0 until n)
      drawPiece(g, Pad+scale(i+1), Pad+SquareSize, nextPieces(i))
    // Grid lines
    g.setColor(GridColour)
    for(i <- 1 to n) drawVLine(g,Pad+scale(i), Pad)
  }

}

