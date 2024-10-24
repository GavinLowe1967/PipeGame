package pipegame

object PipeGame{

  def main(args: Array[String]) = {
    val width = 10; val height = 10 // side of game

    val model = new Model(width, height)
    val frame = new PipeFrame(model)
    model.setFrame(frame)
    frame.peer.setLocationByPlatform(true)
    // frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack(); frame.visible = true
  }
}
 
