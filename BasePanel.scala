package pipegame
import scala.swing._
import java.awt.geom.Arc2D
import java.awt.Color

/** The base class of the main and top panels.  Defines various operations
  * common to each, such as drawing pieces. */
abstract class BasePanel(val width: Int, val height: Int) extends Panel{
  /** Size of one square in pixels. */ 
  protected val SquareSize = 60

  /** Padding around between maze and edge of panel. */
  protected val Pad = 0 // 5

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

  // ===== Drawing straight lines

  private def setPenToDrawPipe(g: Graphics2D) = 
    g.setStroke(new java.awt.BasicStroke(4))

  /** A line from (x1,y1) to (x2,y2). */
  @inline protected def line(x1: Int, y1: Int, x2: Int, y2: Int) =
    new java.awt.geom.Line2D.Float(
      x1.toFloat, y1.toFloat, x2.toFloat, y2.toFloat)

  /** Draw a line from (x1,y1) to (x2,y2). */
  @inline protected
  def drawLine(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int) =
    g.draw(line(x1, y1, x2, y2))
  
  /** Draw a vertical line of length SquareSize upwards from (x,y). */
  @inline protected 
  def drawVLine(g: Graphics2D, x: Int, y: Int) =
    drawLine(g, x, y, x, y+SquareSize)
  
  /** Draw a horizontal line of length SquareSize rightwards from (x,y). */ 
  @inline protected
  def drawHLine(g: Graphics2D, x: Int, y: Int) =
    drawLine(g, x, y, x+SquareSize, y)

  // ===== Drawing arcs.

  /** Draw a quarter-circle pipe, centred on (x,y), between angles from and
    * from+90. */
  private def drawQuarterArc(g: Graphics2D, x: Int, y: Int, from: Int) = {
    g.draw(quarterCircle(x, y, PipeWidth+PipePad, from))
    g.draw(quarterCircle(x, y, PipePad, from))
  }

  /** An arc, centred on (x,y), with radius radius, between angles from and
    * from+angle. */
  private def arc(x: Int, y: Int, radius: Int, from: Int, angle: Int) =
    new Arc2D.Double(
      x-radius, y-radius, 2*radius, 2*radius, from, angle, Arc2D.OPEN) 

  /** A quarter circle, centred on (x,y), with radius radius, between angles
    * from and from+90. */
  private def quarterCircle(x: Int, y: Int, radius: Int, from: Int) =
    arc(x, y, radius, from, 90)
    // new Arc2D.Double(
    //   x-radius, y-radius, 2*radius, 2*radius, from, 90, Arc2D.OPEN) 


  private def quarterPie(x: Int, y: Int, radius: Int, from: Int, angle: Int) = 
    new Arc2D.Double(
      x-radius, y-radius, 2*radius, 2*radius, from, angle, Arc2D.PIE) 


  // ===== Drawing a piece

  /** Draw piece p in a square with bottom-left corner (x,y). */
  protected 
  def drawPiece(g: Graphics2D, x: Int, y: Int, p: Piece) = {
    // println(s"BasePanel.drawPiece $p")
    setPenToDrawPipe(g)
    // g.setStroke(new java.awt.BasicStroke(4))
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
    /* Draw water. */
    // def drawWaterFromS(steps: Int) = {
    //   val height = steps*SquareSize/Piece.FillSteps
    //   g.fillRect(x+PipePad, y-height, PipeWidth, height)
    // }
    // Now pattern match
    p match{
      case NS() => drawWest; drawEast
      case EW() => drawNorth; drawSouth
      case NE() => drawQuarterArc(g, x+SquareSize, y-SquareSize, 180)
      case NW() => drawQuarterArc(g, x, y-SquareSize, 270)
      case SE() => drawQuarterArc(g, x+SquareSize, y, 90)
      case SW() => drawQuarterArc(g, x, y, 0)
      case NES() => drawWest; drawSE; drawNE; drawES; drawEN
      case ESW() => drawNorth; drawES; drawWS; drawSE; drawSW
      case SWN() => drawEast; drawSW; drawNW; drawWS; drawWN
      case WNE() => drawSouth; drawWN; drawEN; drawNE; drawNW
      case Cross() =>
        drawNE; drawNW; drawSE; drawSW; drawWN; drawWS; drawEN; drawES
      case NSOverEW() => drawWest; drawEast; drawWN; drawWS; drawEN; drawES
      case EWOverNS() => drawNorth; drawSouth; drawNE; drawNW; drawSW; drawSE
    }
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }

  /** Draw the water within piece p in a square with bottom-left corner (x,y). */
  protected def fillPipe(g: Graphics2D, x: Int, y: Int, p: Piece) = {
    import Piece.{N,S,E,W,FillSteps}
    g.setStroke(new java.awt.BasicStroke(2))
    /* Draw water along a straight pipe, starting at end `end`, for `steps`
     * steps. */
    def drawWaterFrom(end: Int, steps: Int) = {
      val size = steps*SquareSize/FillSteps
      end match{
        case S => g.fillRect(x+PipePad, y-size, PipeWidth, size)
        case N => g.fillRect(x+PipePad, y-SquareSize, PipeWidth, size)
        case W => g.fillRect(x, y-SquareSize+PipePad, size, PipeWidth) 
        case E => 
          g.fillRect(x+SquareSize-size, y-SquareSize+PipePad, size, PipeWidth)
      }
    }
    /* Draw water along an curved pipe, with arc centre (`xx`,`yy`), starting at
     * angle `from`, for `steps` steps.  A negative value for `steps` means the
     * arc is in a clockwise direction. */
    def drawWaterArc(xx: Int, yy: Int, from: Int, steps: Int) = {
      val angle = 90*steps/FillSteps
      for(r <- PipePad+1 to PipeWidth+PipePad-1)
        g.draw(arc(xx, yy, r, from, angle))
    }
    // Now pattern match on p
    p match{
      case tep: StraightPiece =>
        for(ix <- 0 until 2) drawWaterFrom(tep.ends(ix), tep.filledFrom(ix))
      case ne: NE =>
        drawWaterArc(x+SquareSize, y-SquareSize, 180, ne.filledFrom(0))
        drawWaterArc(x+SquareSize, y-SquareSize, 270, -ne.filledFrom(1))
      case nw: NW => 
        drawWaterArc(x, y-SquareSize, 0, -nw.filledFrom(0))
        drawWaterArc(x, y-SquareSize, 270, nw.filledFrom(1))
      case se: SE =>
        drawWaterArc(x+SquareSize, y, 180, -se.filledFrom(0))
        drawWaterArc(x+SquareSize, y, 90, se.filledFrom(1))
      case sw: SW => 
        drawWaterArc(x, y, 0, sw.filledFrom(0))
        drawWaterArc(x, y, 90, -sw.filledFrom(1))
      case _ => {}
    }
  }
}

// =======================================================

object BasePanel{
  /** Background colour. */
  val BackgroundColour = Color.white

  /** Colour of the grid lines. */
  val GridColour = Color.black

  /** Colour of pieces that have been played. */
  val PipeColour = Color.blue

  /** Colour of the liquid filling the pipes. */
  val WaterColour = Color.red

  /** Colour of the current piece of pipe. */
  val CurrentPipeColour = new Color(150, 0, 255)

  /** Colour of the queued pieces. */
  val QueuedPipeColour = new Color(160, 160, 255) // light blue
}
