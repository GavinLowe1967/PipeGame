package pipegame

import scala.swing._

/** A panel containing information about score, etc. */
class InfoPanel(model: Model) extends BasePanel(model.width, 2){
  /** Font for displaying the score. */
  private val scoreFont = new Font("SansSerif", java.awt.Font.PLAIN, 24)

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.setFont(scoreFont); val metrics = g.getFontMetrics(scoreFont)
    /* Add text with centre at (x,y). */
    def centreText(x: Int, y: Int, text: String) = {
      val xx = x-metrics.stringWidth(text)/2; val yy = y+metrics.getHeight/2
      g.drawString(text, xx, yy)
    }
    val y = Pad+SquareSize/2
    centreText(Pad+scale(2), y, s"Score: ${model.getScore}")
    centreText(Pad+scale(width-2), y, s"Kills left: ${model.getKillsLeft}")
    centreText(Pad+scale(2), y+SquareSize, s"Level: ${model.getLevel}")
  }



}
