package pipegame

import scala.swing._

/** The top panel in the game.  Displays the queue of the next `width` pieces,
  * and the score. */
class TopPanel(width: Int) extends BasePanel(width,2){
  /** The next pieces. */
  private var nextPieces = List[Piece]()

  /** Set the next peices to be ps, and repaint. */
  def setNextPieces(ps: List[Piece]) = { 
    assert(ps.length == width, ps); nextPieces = ps; repaint() 
  }

  /** The current score. */
  private var score = 0

  /** Set the score to s. */
  def setScore(s: Int) = { score = s; repaint() }

  /** Font for displaying the score. */
  private val scoreFont = new Font("SansSerif", java.awt.Font.PLAIN, 24)

  /** Paint this component. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setColor(BackgroundColour)
    g.fillRect(Pad, Pad, scale(width), scale(2))

    // Paint nextPieces
    // val offSet = Pad 
    g.setColor(NextPipeColour)
    drawPiece(g, Pad, Pad+SquareSize, nextPieces(0))
    g.setColor(PipeColour)
    for(i <- 1 until width){
      drawPiece(g, Pad+scale(i), Pad+SquareSize, nextPieces(i))
    }
    // Print score
    g.setFont(scoreFont); val metrics = g.getFontMetrics(scoreFont)
    val text = score.toString
    val xx = Pad+scale(width-1)-metrics.stringWidth(text)/2
    val yy = Pad+3*SquareSize/2+metrics.getHeight/2
    g.drawString(text, xx, yy)
  }
}
