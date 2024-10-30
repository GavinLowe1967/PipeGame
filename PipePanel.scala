package pipegame

import scala.swing._
import scala.swing.event._
import java.awt.Color

import BasePanel._

/** The panel displaying the pipe network. */
class PipePanel(model: Model, frame: FrameT) 
    extends BasePanel(model.width, model.height){

  /* Width and height of the grid. */
  //private val width = model.width; private val height = model.height

  /** Convert grid coordinates to screen coordinates.  Return the screen
    * coordinates of the bottom-left corner of the grid square. */
  private def gridToScreen(x: Int, y: Int): (Int,Int) = 
    (scale(x)+Pad, scale(height-y)+Pad)

  /** Convert screen coordinates to grid position. */
  def screenToGrid(p: Point): (Int,Int) = {
    ((p.x-Pad)/SquareSize, height-(p.y-Pad)/SquareSize-1)
  }

  /** Coordinates of the mouse, or -1 if outside the grid. */
  private var mouseX = -1; private var mouseY = -1 

  /** Paint this component. */
  override def paintComponent(g: Graphics2D) = {
    /* Helper function. */
    /* Draw a line from (x1,y1) to (x2,y2) (in grid coordinates). */
    @inline def drawLine(x1: Int, y1: Int, x2: Int, y2: Int) = {
      val (xx1,yy1) = gridToScreen(x1,y1); val (xx2,yy2) = gridToScreen(x2,y2)
      g.draw(line(xx1, yy1, xx2, yy2))
    }

    super.paintComponent(g)
    g.setColor(BackgroundColour)
    g.fillRect(Pad, Pad, scale(width), scale(height))

    // Draw pipes
    g.setColor(PipeColour)
    for(x <- 0 until width; y <- 0 until height){
      val p = model.grid(x)(y)
      if(p != null){
        val (x1,y1) = gridToScreen(x,y); drawPiece(g, x1, y1, p)
      }
    }

    // Draw current piece
    if(mouseX >= 0 && mouseY >= 0 && model.grid(mouseX)(mouseY) == null){ 
      //println("showing")
      g.setColor(CurrentPipeColour); val (x1,y1) = gridToScreen(mouseX,mouseY)
      drawPiece(g, x1, y1, model.getCurrentPiece)
    }

    // Grid lines
    g.setColor(GridColour)
    for(x <- 0 to width) drawLine(x, 0, x, height)
    for(y <- 0 to height) drawLine(0, y, width, y)
  }

  /** Reactions to mouse and key presses. */
  //def init = {
  focusable = true; requestFocus(); listenTo(mouse.clicks,mouse.moves,keys) 

  // init
  reactions += {
    case KeyPressed(_,c,_,_) => c match{
      case Key.Q => frame.quitFrame; sys.exit()
      case Key.Z => model.rotateLeft(); repaint()
      case Key.X => model.rotateRight(); repaint()
      case Key.K => model.killPiece(); repaint()
      // case Key.Enter => model.endLevel(); repaint()
      case _ => {}
    }
    case e: MouseClicked => 
      val (x,y) = screenToGrid(e.point)
      buttonOf(e) match{
        case 1 => model.playAt(x,y); repaint()
        case _ => {}
      }

    case e: MouseMoved => 
      val (x,y) = screenToGrid(e.point)
      if(0 <= x && x < width && 0 <= y && y < height){
        // Don't repaint if no change
        if(x != mouseX || y != mouseY){ mouseX = x; mouseY = y; repaint() }
      }
      else{ mouseX = -1; mouseY = -1 }
  }

  /** Extract the button from a MouseClicked event. */
  def buttonOf(e: MouseClicked): Int = e.peer.getButton match{
      case java.awt.event.MouseEvent.BUTTON1 => 1
      case java.awt.event.MouseEvent.BUTTON2 => 2
      case java.awt.event.MouseEvent.BUTTON3 => 3
      case _ => println("Unknown button"); -1
    }
}
