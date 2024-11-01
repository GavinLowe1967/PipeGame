package pipegame

object Concurrency{
  /** Create a thread that, when run, will execute `comp`. */
  private def mkThread(comp: => Unit) =
    new Thread(new Runnable{ def run = comp })

  /** Run a thread to perform `comp`. */
  def runThread(comp: => Unit) = mkThread(comp).start
}
