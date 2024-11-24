package pipegame

import scala.swing._
import javax.swing.{JScrollPane,ScrollPaneConstants}

/** The main frame of the game. */
class PipeFrame(model: Model) extends MainFrame with FrameT{

  /** The panel which displays the main playing area. */
  private val panel = new PipePanel(model, this)

  /** The top panel, displaying the next piece. */
  private val topPanel = new TopPanel(model)

  private val scrollTopPanel = new ScrollPane(topPanel){ 
    preferredSize = new Dimension(600, 80) 
  }

  /** Set the next piece to be p. */
  def setNextPieces(ps: List[Piece]) = topPanel.setNextPieces(ps) 

  /** Panel displaying score. etc. */
  private val infoPanel = new InfoPanel(model)

  /* ----- radio buttons ----- */

  private val radioButtonFont = new Font("SansSerif", java.awt.Font.PLAIN, 20)
  private val placeButton = new RadioButton("Place"){
    font = radioButtonFont
    action = Action("Place"){ panel.setKillMode(false) }
  }

  private val killButton = new RadioButton("Kill"){ 
    font = radioButtonFont
    action = Action("Kill"){ panel.setKillMode(true) }
  }

  private val buttongroup = new ButtonGroup {
    buttons += placeButton; buttons += killButton
    select(placeButton)
  }

  def updateInfo() = infoPanel.repaint()

  def update() = panel.repaint() 

  /* Add the components to this. */
  contents = new BoxPanel(Orientation.Vertical) {  
    import Swing._
    border = EmptyBorder(10)
    contents ++= List(scrollTopPanel, VStrut(5))
    // InfoPanel and radio buttons
    contents += new BoxPanel(Orientation.Horizontal){
      contents ++= List(HStrut(10), infoPanel, HGlue)
      contents += new BoxPanel(Orientation.Vertical) {
        contents ++= List(placeButton, killButton)
      }
      contents += HStrut(20)
    }
 
    // Main grid
    contents += new BoxPanel(Orientation.Horizontal){ 
      border = EmptyBorder(5)
      contents ++= List(HStrut(20), HGlue, HGlue, panel, HGlue, HStrut(20))
    }
  }

  // The radioButtons seem to grab the focus.
  panel.setFocusable()

  import javax.swing.WindowConstants.EXIT_ON_CLOSE
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  def quitFrame = { println("quitting"); close(); dispose(); sys.exit() }

  /** Update whether the pipes are currently being filled. */
  def setFilling(f: Boolean) = panel.setFilling(f)
}
