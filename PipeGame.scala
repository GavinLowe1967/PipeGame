package pipegame

object PipeGame{

  def main(args: Array[String]) = {
    val width = 7; val height = 7 // side of game

    val model = new Model(width, height)
    val frame = new PipeFrame(model)
    model.init(frame)
    frame.peer.setLocationByPlatform(true)
    frame.location = new scala.swing.Point(500,50)
    // frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack(); frame.visible = true
  }
}
 
