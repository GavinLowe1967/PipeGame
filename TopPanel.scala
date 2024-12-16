package pipegame

import BasePanel._

import scala.swing._

class TopPanel(model: Model) extends BasePanel{
   // preferredSize = 
   //   new Dimension(model.width*model.height*SquareSize, SquareSize)

  /** The next pieces. */
  private var nextPieces = List[Piece]()

  /** Number of next pieces.  Note: this excludes the current piece. */
  private var n = 0

  /** Set the next peices to be ps, and repaint. */
  def setNextPieces(ps: List[Piece]) = {
    nextPieces = ps; n = ps.length; setSize(n+1,1); repaint()
    // println(s"n = $n")
  }

  /** Paint this component. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setColor(BackgroundColour)
    g.fillRect(Pad, Pad, scale(n+1), scale(2))

    // Paint nextPieces
    // val offSet = Pad; 
    val textY = Pad+scale(1.15) // y coord for numbers
    g.setColor(CurrentPipeColour)
    drawPiece(g, Pad, Pad+SquareSize, model.getCurrentPiece) //  nextPieces(0))
    centreText(g, Pad+scale(0.5), textY, "1")
    g.setColor(QueuedPipeColour)
    for(i <- 0 until n){
      drawPiece(g, Pad+scale(i+1), Pad+SquareSize, nextPieces(i))
      centreText(g, Pad+scale(i+1.5), textY, (i+2).toString)
    }
    // Grid lines
    g.setColor(GridColour)
    for(i <- 1 to n) drawVLine(g,Pad+scale(i), Pad)
    drawLine(g, Pad, Pad+scale(1), Pad+scale(n+1), Pad+scale(1))
  }

}

