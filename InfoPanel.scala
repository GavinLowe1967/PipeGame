package pipegame

import scala.swing._

/** A panel containing information about score, etc. */
class InfoPanel(model: Model) extends BasePanel{
  val width = model.width-1

  setSize(width, 2)

  /** Font for displaying the score. */
  private val scoreFont = new Font("SansSerif", java.awt.Font.PLAIN, 24)

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setFont(scoreFont); val metrics = g.getFontMetrics(scoreFont)
    val y = Pad+SquareSize/2
    centreText(g, Pad+scale(1.5), y, s"Score: ${model.getScore}")
    centreText(g, Pad+scale(width-2), y, s"Kills left: ${model.getKillsLeft}")
    centreText(g, Pad+scale(1), y+SquareSize, s"Level: ${model.getLevel}")
  }



}
