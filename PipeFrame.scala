package pipegame

import scala.swing._

/** The main frame of the game. */
class PipeFrame(model: Model) extends MainFrame with FrameT{

  /** The panel which displays the main playing area. */
  private val panel = new PipePanel(model, this)

  /** The top panel, displaying the next piece. */
  private val topPanel = new TopPanel(model.width)

  /** Set the next piece to be p. */
  def setNextPieces(ps: List[Piece]) = { 
    topPanel.setNextPieces(ps) //; panel.repaint 
  }

  // private val scorePanel = new TextField{
  //   //editable = false; font = new Font("SansSerif", java.awt.Font.PLAIN, 16)
  //   // rows = 2
  // }

  def setScore(score: Int) = topPanel.setScore(score) //  println(score) }

  /* Add the components to this. */
  contents = new BoxPanel(Orientation.Vertical) {  
    border = Swing.EmptyBorder(10)
    contents += topPanel      
    contents += Swing.HStrut(50)
    contents += panel
  }

  import javax.swing.WindowConstants.EXIT_ON_CLOSE
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  def quitFrame = { println("quitting"); close(); dispose(); sys.exit() }
}
