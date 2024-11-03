package pipegame

object PipeGame{

  val width = 7; val height = 7 // size of game

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; var level = 1
    while(i < args.length) args(i) match{
      case "--level" => level = args(i+1).toInt; i += 2
    }

    val model = new Model(width, height, level)
    val frame = new PipeFrame(model)
    model.init(frame)
    frame.peer.setLocationByPlatform(true)
    frame.location = new scala.swing.Point(500,50)
    // frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack(); frame.visible = true
  }
}
 
