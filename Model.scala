package pipegame

import scala.util.Random

class Model(val width: Int, val height: Int){
  import Piece.{NumShapes,FillSteps}

  private val random = 
    new Random(Random.nextInt() ^ java.lang.System.currentTimeMillis)

  /** Total number of squares. */
  private val size = width*height

  /** The frame.  Set by the MazeGame object. */
  private var frame: FrameT = null

  /** Set the frame to be f, and initialise the first level. */
  def init(f: FrameT) = { frame = f; initLevel() }

  /** The grid. */
  val grid = Array.ofDim[Piece](width, height)

  /* The source and sink of the flow. */
  private var sourceX = -1
  private var sinkX = -1 //  = (-1,height-1)

  /** The number of next pieces in the queue. */
  val NumNextPieces = 8

  /** The next pieces.  */
  private var nextPieces: List[Piece] = List[Piece]()

  @volatile private var currentPiece: Piece = null

  /** Get the current piece.  Called by the PipePanel and TopPanel. */
  def getCurrentPiece = currentPiece 

  /** The current level. */
  private var level = 0

  /** Get the current level.  Called by InfoPanel. */
  def getLevel = level

  /** Pick a Piece at random. */
  private def getPiece(): Piece = {
    val p = nextPieces.head; nextPieces = nextPieces.tail; p
  }

  /** Initialise the next level. */
  private def initLevel() = {
    import Piece.{N,S,E,W}
    level += 1
    for(x <- 0 until width; y <- 0 until height) grid(x)(y) = null 
    initNextPieces()
    // Choose source and sink pieces and positions, ensuring level is feasible
    sourceX = Random.nextInt(width); var p = getPiece()
    while(!p.ends.contains(S) || (sourceX == 0 && p.ends.contains(W)) ||
        (sourceX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = getPiece()
      // If we can't place p here, we add it to the end of the queue; but we
      // rotate it to guard against the case that no piece can be placed here.
    }
    grid(sourceX)(0) = p
    sinkX = Random.nextInt(width); p = getPiece()
    while(!p.ends.contains(N) || (sinkX == 0 && p.ends.contains(W)) ||
        (sinkX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = getPiece()
    }
    grid(sinkX)(height-1) = p
    currentPiece = getPiece()
    // println(nextPieces.mkString(", "))
    frame.setNextPieces(nextPieces)
  }

  /** Initialise nextPieces to length `size`, with pieces following the
    * proportions for this level, as near as possible. */
  private def initNextPieces() = {
    import LevelInfo._
    val LevelInfo(probs) = 
      if(level <= levels.length) levels(level-1) else defaultLevel
    // Each shape, with index ix, should appear roughly ideals(ix) =
    // size*probs(ix) times: either the floor or the ceiling of that number.
    val ideals = Array.tabulate[Double](NumShapes)(ix => size*probs(ix))
    // println(ideals.map(_.toString).mkString(", "))
    val remainders = new Array[Double](NumShapes) 
    // `remainders` will hold the fractional parts of `ideals`.  `pieces` are
    // the pieces chosen so far.  `len = pieces.length`.
// IMPROVE: use array for pieces
    var pieces = List[Piece](); var len = 0
    for(ix <- 0 until NumShapes){
      // Add k = floor(ideals(ix)) shapes from shapeClasses(ix) to nextPieces.
      val k = ideals(ix).toInt; len += k; remainders(ix) = ideals(ix)-k
      pieces = List.fill(k)(choosePiece(ix)) ++ pieces
    }
    assert(len <= size && size-len < NumShapes)
    // Add extra shapes from classes in extras: those with largest remainders.
    val extras = (0 until NumShapes).sortBy(ix => -remainders(ix)).take(size-len)
    for(ix <- extras) pieces ::= choosePiece(ix)
    // Shuffle pieces into nextPieces
    nextPieces = List(); len = size // Inv: len = pieces.length
    while(len > 0){
      val k = random.nextInt(len); val (pref, p::suf) = pieces.splitAt(k)
      nextPieces ::= p; pieces = pref++suf; len -= 1
    }
    assert(pieces.isEmpty && nextPieces.length == size) 
    //; println(nextPieces.mkString(", "))
  }

  /** Choose a random piece from shapeClasses(ix). */
  @inline private def choosePiece(ix: Int): Piece = {
    val ps = Piece.shapeClasses(ix); ps(random.nextInt(ps.length))()
  }

  /** Update state to move to the next piece. */
  private def getNextPiece() = {
    currentPiece = nextPieces.head; 
    nextPieces = nextPieces.tail :+ getPiece() 
    frame.setNextPieces(nextPieces)
  }

  /** The current score. */
  private var score = 0

  /** Get the current score.  Called by the InfoPanel. */
  def getScore = score

  /** Add s to the score. */
  private def addScore(s: Int) = { score += s; frame.updateInfo() }

  /** Number of kills still allowed. */
  private var killsLeft = 5

  /** Get the number of kills left.  Called by the InfoPanel. */
  def getKillsLeft = killsLeft

  // ========= Main external functions =========

  /** Is this level over? */
  private def isLevelOver: Boolean = {
    // Test if every pipe end is connected
    for(x <- 0 until width; y <- 0 until height){
      val p = grid(x)(y)
      if(p != null) 
        for((dx,dy) <- p.deltas){
          val xx = x+dx; val yy = y+dy
          // There should be a piece at (xx,yy)
          if(xx < 0 || xx >= width ||
            (if(yy < 0) x != sourceX else if(yy >= height) x != sinkX
            else grid(xx)(yy) == null)
          ) return false
        }
    } // end of for loop
// FIXME: also check connected
    true
  }

  /** Animate the filling of the pipes. */
  private def fillPipes() = {
    println("End of level")
    /* We perform a breadth-first traversal.  At each ply, we fill all pieces in
     * the current ply, then move to the next ply.  `frontier` holds the
     * current frontier of the traversal, and `seen` the coordinates seen
     * previously. */ 
    var frontier = List((sourceX,0))
    grid(sourceX)(0).enterFrom(Piece.S)
    val seen = Array.ofDim[Boolean](width, height); seen(sourceX)(0) = true
    while(frontier.nonEmpty){ 
      // println(frontier.map(_.toString).mkString(", "))
      // Animate filling.
      val frontierPs = frontier.map{ case(x,y) => grid(x)(y) }
      for(i <- 0 until FillSteps){
        for(p <- frontierPs) p.fillStep()
        Thread.sleep(200/FillSteps); frame.update() // IMPROVE
      }
      // Find frontier for next iteration
      var newFrontier = List[(Int,Int)]()
      for((x,y) <- frontier; end <- grid(x)(y).ends){
        val (dx,dy) = Piece.endToDelta(end); val xx = x+dx; val yy = y+dy
        if(yy == height) assert((x,y) == (sinkX,height-1))
        else if(yy < 0) assert((x,y) == (sourceX,0))
        else if(!seen(xx)(yy)){     // water can flow from (x,y) into (xx)(yy)
          grid(xx)(yy).enterFrom(Piece.reverse(end)); newFrontier ::= ((xx,yy))
        }
      } // end of for loop
      frontier = newFrontier.distinct
      for((xx,yy) <- frontier) seen(xx)(yy) = true
      // Note: we update `seen` only at the end of the ply, because we might
      // want water to enter a particular piece from two directions.
    } // end of while(frontier.nonEmpty)
  }

  /** Play at (x,y). */
  def playAt(x: Int, y: Int) = if(grid(x)(y) == null){
    grid(x)(y) = currentPiece; addScore(currentPiece.score)
    if(isLevelOver) Concurrency.runThread{ 
      fillPipes(); Thread.sleep(500); initLevel(); frame.update()
    }
    else getNextPiece()
  }

  /** Kill the current piece. */
  def killPiece() = {
    if(killsLeft > 0){
      // Apply penalty equal to the piece's value.
      addScore(-currentPiece.score)
      // Replace at end of queue
      nextPieces = nextPieces :+ currentPiece
      killsLeft -= 1; frame.updateInfo()
      getNextPiece()
    }
    else println("No kills left") // IMPROVE
  }

  /** Rotate the next piece left. */
  def rotateLeft() = currentPiece =currentPiece.rotateLeft

  /** Rotate the next piece right. */
  def rotateRight() = currentPiece = currentPiece.rotateRight


}

// =======================================================

object Model{
  // private val random = 
  //   new Random(Random.nextInt() ^ java.lang.System.currentTimeMillis)
}
