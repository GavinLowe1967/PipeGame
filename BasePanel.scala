package pipegame
import scala.swing._
import java.awt.geom.Arc2D
import java.awt.Color

/** The base class of the main and top panels.  Defines various operations
  * common to each, such as drawing pieces. */
abstract class BasePanel(width: Int, height: Int) extends Panel{
  /** Size of one square in pixels. */ 
  protected val SquareSize = 50

  /** Padding around between maze and edge of panel. */
  protected val Pad = 5

  /** The with of a pipe. */
  private val PipeWidth = 16

  /* SquareSize-PipeWidth must be even. */

  /** The gap between the edge of a square and a pipe. */
  private val PipePad = (SquareSize-PipeWidth)/2

  assert(PipeWidth+2*PipePad == SquareSize)

  /** The distance from the edge of a square to the far side of a pipe. */
  private val PipeEnd = PipeWidth+PipePad

  /** Scale grid coordinate to pixels. */
  @inline protected def scale(x: Int) = x*SquareSize

  preferredSize = new Dimension(scale(width)+2*Pad, scale(height)+2*Pad)
  minimumSize = preferredSize

  protected val PipeColour = Color.blue

  protected val BackgroundColour = Color.white

  protected val NextPipeColour = new Color(160, 160, 255)

  /** A line from (x1,y1) to (x2,y2). */
  @inline protected def line(x1: Int, y1: Int, x2: Int, y2: Int) =
    new java.awt.geom.Line2D.Float(
      x1.toFloat, y1.toFloat, x2.toFloat, y2.toFloat)

  /** Draw a line from (x1,y1) to (x2,y2). */
  @inline protected
  def drawLine(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int) =
    g.draw(line(x1, y1, x2, y2))
  
  /** Draw a vertical line of length SquareSize upwards from (x,y). */ 
  def drawVLine(g: Graphics2D, x: Int, y: Int) =
    drawLine(g, x, y, x, y+SquareSize)
  
  /** Draw a horizontal line of length SquareSize rightwards from (x,y). */ 
  def drawHLine(g: Graphics2D, x: Int, y: Int) =
    drawLine(g, x, y, x+SquareSize, y)

  /** Draw piece p in a square with bottom-left corner (x,y). */
  protected 
  def drawPiece(g: Graphics2D, x: Int, y: Int, p: Piece) = {
    g.setStroke(new java.awt.BasicStroke(4))
    /* Full lines, to the north, south, east, west of the pipe, respectively. */
    def drawNorth = drawHLine(g, x, y-SquareSize+PipePad)
    def drawSouth = drawHLine(g, x, y-SquareSize+PipePad+PipeWidth)
    def drawEast = drawVLine(g, x+PipePad+PipeWidth, y-SquareSize)
    def drawWest = drawVLine(g, x+PipePad, y-SquareSize)
    /* Partial strokes: the first discriminator indicates the direction of the
     * pipe part from the centre; the second indicates the direction of this
     * wall from that pipe part. */
    def drawNE = drawLine(
      g, x+PipeEnd, y-SquareSize, x+PipeEnd, y-SquareSize+PipePad)
    def drawSE = drawLine(g, x+PipeEnd, y-PipePad, x+PipeEnd, y)
    def drawNW = drawLine(
      g, x+PipePad, y-SquareSize, x+PipePad, y-SquareSize+PipePad)
    def drawSW = drawLine(g, x+PipePad, y-PipePad, x+PipePad, y)
    def drawEN = drawLine(
      g, x+PipeEnd, y-SquareSize+PipePad, x+SquareSize, y-SquareSize+PipePad)
    def drawES = drawLine(g, x+PipeEnd, y-PipePad, x+SquareSize, y-PipePad)
    def drawWN = drawLine(g, x, y-PipeEnd, x+PipePad, y-PipeEnd)
    def drawWS = drawLine(g, x, y-PipePad, x+PipePad, y-PipePad)
    // Now pattern match
    p match{
      case NS => drawWest; drawEast
      case EW => drawNorth; drawSouth
      case NE => drawQuarterArc(g, x+SquareSize, y-SquareSize, 180)
      case NW => drawQuarterArc(g, x, y-SquareSize, 270)
      case SE => drawQuarterArc(g, x+SquareSize, y, 90)
      case SW => drawQuarterArc(g, x, y, 0)
      case NES => drawWest; drawSE; drawNE; drawES; drawEN
      case ESW => drawNorth; drawES; drawWS; drawSE; drawSW
      case SWN => drawEast; drawSW; drawNW; drawWS; drawWN
      case WNE => drawSouth; drawWN; drawEN; drawNE; drawNW
      case Cross =>
        drawNE; drawNW; drawSE; drawSW; drawWN; drawWS; drawEN; drawES
      case NSOverEW => drawWest; drawEast; drawWN; drawWS; drawEN; drawES
      case EWOverNS => drawNorth; drawSouth; drawNE; drawNW; drawSW; drawSE
    }
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }

  /** Draw a quarter-arc pipe, centred on (x,y), between angles from and
    * from+90. */
  private def drawQuarterArc(g: Graphics2D, x: Int, y: Int, from: Int) = {
    //g.setColor(PipeColour); 
    g.draw(quarterCircle(x, y, PipeWidth+PipePad, from))
    g.draw(quarterCircle(x, y, PipePad, from))
    // g.fill(quarterCircle(x, y, PipeWidth+PipePad, from))
    // g.setColor(BackgroundColour); g.fill(quarterCircle(x, y, PipePad, from))
  }

  /** A quarter circle, centred on (x,y), with radius radius, between angles
    * from and from+90. */
  private def quarterCircle(x: Int, y: Int, radius: Int, from: Int) = 
    new Arc2D.Double(
      x-radius, y-radius, 2*radius, 2*radius, from, 90, Arc2D.OPEN) //PIE
  

}
