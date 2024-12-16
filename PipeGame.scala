package pipegame

object PipeGame{

  val width = 8; val height = 8 // size of game

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; var level = 1; var adjustment = 5
    while(i < args.length) args(i) match{
      case "--level" => level = args(i+1).toInt; i += 2
      case "--hard" => adjustment = 10; i += 1
      case "--easy" => adjustment = 0
    }

    LevelInfo.init(adjustment)
    val model = new Model(width, height, level)
    val frame = new PipeFrame(model)
    model.init(frame)
    frame.peer.setLocationByPlatform(true)
    frame.location = new scala.swing.Point(500,50)
    // frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack(); frame.visible = true
  }
}
 
