package pipegame
import scala.swing._
import java.awt.geom.Arc2D
import java.awt.Color

/** The base class of the main and top panels.  Defines various operations
  * common to each, such as drawing pieces. */
abstract class BasePanel extends Panel{
  val SquareSize = BasePanel.SquareSize

  /** Padding around between grid and edge of panel. */
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
  @inline def scale(x: Int) = x*SquareSize

  /** Scale grid coordinate to pixels. */
  @inline def scale(x: Double) = (x*SquareSize).toInt

  /** Set the size of the Panel, in square units. */
  def setSize(width: Int, height: Int) = {
    preferredSize = new Dimension(scale(width)+2*Pad, scale(height)+2*Pad)
    minimumSize = preferredSize
  }

  /* Add text with centre at (x,y). */
  def centreText(g: Graphics2D, x: Int, y: Int, text: String) = {
    val metrics = g.getFontMetrics(g.getFont)
    val xx = x-metrics.stringWidth(text)/2; val yy = y+metrics.getHeight/2
    g.drawString(text, xx, yy)
  }

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

  // ===== Drawing a piece

  /** Draw piece p in a square with bottom-left corner (x,y). */
  protected 
  def drawPiece(g: Graphics2D, x: Int, y: Int, p: Piece) = {
    // println(s"BasePanel.drawPiece $p")
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

    setPenToDrawPipe(g)
    // Now pattern match
    p match{
      case NS() => drawWest; drawEast
      case EW() => drawNorth; drawSouth
      case cp: CurvedPiece => 
        drawQuarterArc(g, x+cp.ccdx*SquareSize, y-cp.ccdy*SquareSize, cp.angle)
      // T-pieces
      case NES() => drawWest; drawSE; drawNE; drawES; drawEN
      case ESW() => drawNorth; drawES; drawWS; drawSE; drawSW
      case SWN() => drawEast; drawSW; drawNW; drawWS; drawWN
      case WNE() => drawSouth; drawWN; drawEN; drawNE; drawNW
      // Crossroads and cross-overs
      case Cross() =>
        drawNE; drawNW; drawSE; drawSW; drawWN; drawWS; drawEN; drawES
      case NSOverEW() => drawWest; drawEast; drawWN; drawWS; drawEN; drawES
      case EWOverNS() => drawNorth; drawSouth; drawNE; drawNW; drawSW; drawSE
    }
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }

  // ===== Filling a piece with water

  /** The size, in screen coordinates, of `steps` steps of filling. */
  @inline private def sizeFor(steps: Int) = steps*SquareSize/Piece.FillSteps

  /** Draw the water within piece p in a square with bottom-left corner (x,y). */
  protected def fillPipe(g: Graphics2D, x: Int, y: Int, p: Piece) = {
    import Piece.{N,S,E,W,FillSteps}

    /* Draw water along a straight pipe, starting at end `end`, for `steps`
     * steps. */
    def drawWaterFrom(end: Int, steps: Int) = {
      val size = sizeFor(steps)
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

    /* Draw water from the centre of a pipe towards end `end`, for `steps`
     * steps. */
    def drawWaterTowards(end: Int, steps: Int) = {
      val size = sizeFor(steps)
      end match{
        case S => g.fillRect(x+PipePad, y-SquareSize/2, PipeWidth, size)
        case N => g.fillRect(x+PipePad, y-SquareSize/2-size, PipeWidth, size)
        case E => 
          g.fillRect(x+SquareSize/2, y-SquareSize+PipePad, size, PipeWidth)
        case W => 
          g.fillRect(x+SquareSize/2-size, y-SquareSize+PipePad, size, PipeWidth)
      }
    }

    /* Draw water along the underside of a cross-over piece. */
    def drawWaterUnderpass(end: Int, steps: Int) = {
      val size = sizeFor(steps)
      val size1 = size min PipePad // size up to underpass
      val beyond = size-PipePad-PipeWidth // size beyond underpass
      end match{
        case S => 
          g.fillRect(x+PipePad, y-size1, PipeWidth, size1)
          if(beyond > 0) g.fillRect(x+PipePad, y-size, PipeWidth, beyond)
        case N => 
          g.fillRect(x+PipePad, y-SquareSize, PipeWidth, size1)
          if(beyond > 0) g.fillRect(x+PipePad, y-PipePad, PipeWidth, beyond)
        case W => 
          g.fillRect(x, y-SquareSize+PipePad, size1, PipeWidth)
          if(beyond > 0) g.fillRect(
            x+PipePad+PipeWidth, y-SquareSize+PipePad, beyond, PipeWidth)
        case E =>
          g.fillRect(x+SquareSize-size1, y-SquareSize+PipePad, size1, PipeWidth)
          if(beyond > 0) g.fillRect(
            x+SquareSize-size, y-SquareSize+PipePad, beyond, PipeWidth)
      }
    }

    g.setStroke(new java.awt.BasicStroke(2))
    // Now pattern match on p
    p match{
      case _: StraightPiece =>
        for(ix <- 0 until 2) drawWaterFrom(p.ends(ix), p.filledFrom(ix))

      case cp: CurvedPiece =>
        val ccx = x+cp.ccdx*SquareSize; val ccy = y-cp.ccdy*SquareSize
        drawWaterArc(ccx, ccy, cp.angle, cp.filledFrom(0))
        drawWaterArc(ccx, ccy, cp.angle+90, -cp.filledFrom(1))

      case _: TJunctionPiece =>
        // Draw from ends inwards
        for(ix <- 0 until 3)
          drawWaterFrom(p.ends(ix), p.filledFrom(ix) min FillSteps/2)
        val maxFill = p.filledFrom.max
        // Draw outwards from middle
        if(maxFill > FillSteps/2) 
          for(ix <- 0 until 3) 
            drawWaterTowards(p.ends(ix), maxFill-FillSteps/2)

      case Cross() =>
        for(ix <- 0 until 4) drawWaterFrom(p.ends(ix), p.filledFrom(ix))
        val maxFill = p.filledFrom.max
        // Draw outwards from middle
        if(maxFill > FillSteps/2) 
          for(ix <- 0 until 4) drawWaterTowards(p.ends(ix), maxFill-FillSteps/2)

      case cop: CrossOverPiece =>
        for(ix <- 0 until 4){
          val end = cop.ends(ix)
          if(cop.overEnds.contains(end))
            drawWaterFrom(end, cop.filledFrom(ix))
          else drawWaterUnderpass(end, cop.filledFrom(ix))
        }
    }
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }

  private val ObsOffset = 10

  /** Draw an obstacle in square with bottom-left corner (x, y). */
  protected def drawObstacle(g: Graphics2D, x: Int, y: Int) = {
    setPenToDrawPipe(g)
    val x1 = x+ObsOffset; val x2 = x+SquareSize-ObsOffset
    val y1 = y-ObsOffset; val y2 = y-SquareSize+ObsOffset
    drawLine(g, x1, y1, x2, y2); drawLine(g, x1, y2, x2, y1)
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }

  private val KillOffset = 20

  /** Draw an obstacle in square with bottom-left corner (x, y). */
  protected def drawKill(g: Graphics2D, x: Int, y: Int) = {
    g.setColor(BasePanel.KillSymbolColour); setPenToDrawPipe(g)
    val x1 = x+KillOffset; val x2 = x+SquareSize-KillOffset
    val y1 = y-KillOffset; val y2 = y-SquareSize+KillOffset
    drawLine(g, x1, y1, x2, y2); drawLine(g, x1, y2, x2, y1)
    g.setStroke(new java.awt.BasicStroke(1)) // reset
  }
}

// =======================================================

object BasePanel{
  /** Size of one square in pixels. */ 
  val SquareSize = 60

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

  /** Colour of kill symbol. */
  val KillSymbolColour = new Color(255, 0, 0)
}
