package pipegame

import scala.swing._

/** The main frame of the game. */
class PipeFrame(model: Model) extends MainFrame with FrameT{

  /** The panel which displays the main playing area. */
  private val panel = new PipePanel(model, this)

  /** The top panel, displaying the next piece. */
  private val topPanel = new TopPanel(model)

  /** Set the next piece to be p. */
  def setNextPieces(ps: List[Piece]) = topPanel.setNextPieces(ps)

  /** Panel displaying score. etc. */
  private val infoPanel = new InfoPanel(model)

  def updateInfo() = infoPanel.repaint()

  def update() = panel.repaint() 

  /* Add the components to this. */
  contents = new BoxPanel(Orientation.Vertical) {  
    import Swing._
    border = EmptyBorder(10)
    contents ++= List(topPanel, VStrut(5), infoPanel, VStrut(5))
    contents += new BoxPanel(Orientation.Horizontal){ 
      border = EmptyBorder(5)
      contents ++= List(HStrut(20), HGlue, HGlue, panel, HGlue, HStrut(20))
    }
  }

  import javax.swing.WindowConstants.EXIT_ON_CLOSE
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  def quitFrame = { println("quitting"); close(); dispose(); sys.exit() }
}
