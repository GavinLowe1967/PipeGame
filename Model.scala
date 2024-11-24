package pipegame

import scala.util.Random
import scala.collection.mutable.Stack

class Model(val width: Int, val height: Int, level0: Int, adjustment: Int){
  import Piece.{NumShapes,FillSteps}

  private val random = 
    new Random(Random.nextInt() ^ java.lang.System.currentTimeMillis)

  /** Total number of squares. */
  val size = width*height

  /** The frame.  Set by the MazeGame object. */
  private var frame: FrameT = null

  /** Set the frame to be f, and initialise the first level. */
  def init(f: FrameT) = { frame = f; initLevel() }

  /** The grid. */
  val grid = Array.ofDim[GameObject](width, height)

  /** Y coordinates of sink and source. */
  private val SourceY = 0; private val SinkY = height-1

  /* The source and sink of the flow. */
  private var sourceX = -1
  private var sinkX = -1 //  = (-1,height-1)

  def source = (sourceX, SourceY)
  def sink = (sinkX, SinkY)

  /** The number of next pieces in the queue. */
  val NumNextPieces = 8

  /** The next pieces.  */
  private var nextPieces: List[Piece] = List[Piece]()

  @volatile private var currentPiece: Piece = null

  /** Get the current piece.  Called by the PipePanel and TopPanel. */
  def getCurrentPiece = currentPiece 

  /** The current level. */
  private var level = level0

  /** Get the current level.  Called by InfoPanel. */
  def getLevel = level

  /** Pick a Piece at random. */
  private def getPiece(): Piece = {
    val p = nextPieces.head; nextPieces = nextPieces.tail; p
  }


  /** Are (x1,y1) and (x2,y2) near to one another, within at most 1, both
    * horizontally and vertically? */
  private def near(x1: Int, y1: Int, x2: Int, y2: Int) : Boolean = 
    Math.abs(x1-x2) <= 1 && Math.abs(y1-y2) <= 1

  /** Initialise the next level. */
  private def initLevel(): Unit = {
    import Piece.{N,S,E,W}
    val levelInfo = LevelInfo.get(level+adjustment)
    println(levelInfo)
    // Clear board
    for(x <- 0 until width; y <- 0 until height) grid(x)(y) = null
    // Initialise pieces
    initNextPieces(levelInfo); currentPiece = getPiece()

    // Choose source and sink pieces and positions, ensuring level is feasible
    sourceX = Random.nextInt(width); var p = getPiece()
    while(!p.ends.contains(S) || (sourceX == 0 && p.ends.contains(W)) ||
      (sourceX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = getPiece()
      // If we can't place p here, we add it to the end of the queue; but we
      // rotate it to guard against the case that no piece can be placed
      // here.
    }
    grid(sourceX)(SourceY) = p
    sinkX = Random.nextInt(width); p = getPiece()
    while(!p.ends.contains(N) || (sinkX == 0 && p.ends.contains(W)) ||
      (sinkX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = getPiece()
    }
    grid(sinkX)(SinkY) = p

    // Set obstacles
    val numObstacles = levelInfo.numObstacles
    for(i <- 0 until numObstacles){
      var x = Random.nextInt(width); var y = Random.nextInt(height)
      while(grid(x)(y) != null || near(x, y, sourceX, SourceY) ||
        near(x, y, sinkX, SinkY)){
        x = Random.nextInt(width); y = Random.nextInt(height)
      }
      // Place obstacle at (x,y)
      grid(x)(y) = Obstacle
    }

    // If not possible, try again
    if(!isPossible(source, sink)) initLevel()
    else{    // Update frame
      frame.setFilling(false); frame.setNextPieces(nextPieces)
    }
  }

  type Coord = (Int,Int)

  /** The neighbouring coordinates of the piece at `coord`.  Note: these might
    * include coordinates off the board. */
  // private def neighbours(coord: Coord): List[Coord] = {
  //   val (x,y) = coord
  //   grid(x)(y).asPiece.deltas.map{ case (dx,dy) => (x+dx, y+dy) }
  // }

  /** Is there a route from `source` to `sink`, avoiding obstacles? */
  private def isPossible(source: Coord, sink: Coord): Boolean = {
    // Note: we use the fact that no obstacle is adjacent to the source and
    // sink pieces, so we don't have to obey the movements implied by those
    // pieces.
    // Stack of coordinates to expand
    val stack = new Stack[Coord]; stack.push(source) 
    // Coordinates seen so far
    val seen = Array.ofDim[Boolean](width, height)
    seen(source._1)(source._2) = true
    var done = false // true if reached sink

    while(!done && stack.nonEmpty){
      val (x,y) = stack.pop()
      if(sink == (x,y)) done = true
      else
        for((x1,y1) <- List((x+1,y), (x-1,y), (x,y+1), (x,y-1)))
          if(x1 >= 0 && x1 < width && y1 >= 0 && y1 < height && !seen(x1)(y1) &&
              grid(x1)(y1) != Obstacle){
            stack.push((x1,y1)); seen(x1)(y1) = true
          }
    } // end of while loop
    if(!done){
      for(y <- height-1 to 0 by -1){
        for(x <- 0 until width) print(grid(x)(y) match{
          case null => " "; case Obstacle => "X"; case p: Piece => "*"
        })
        println()
      }
    }
    done
  }

  private def nextLevel() = { 
    level += 1; killsLeft += 1; frame.updateInfo(); initLevel() }

  /** Initialise nextPieces to length `size`, with pieces following the
    * proportions for this level, as near as possible. */
  private def initNextPieces(levelInfo: LevelInfo) = {
    import LevelInfo._
    val probs = levelInfo.probs; val numPieces = size-levelInfo.numObstacles
    // Each shape, with index ix, should appear roughly ideals(ix) =
    // numPieces*probs(ix) times: either the floor or the ceiling of that number.
    val ideals = Array.tabulate[Double](NumShapes)(ix => numPieces*probs(ix))
    // println(ideals.map(_.toString).mkString(", "))
    val remainders = new Array[Double](NumShapes) 
    // `remainders` will hold the fractional parts of `ideals`.
    // `pieces[0..n)` are the pieces chosen so far.
    var pieces = new Array[Piece](numPieces); var n = 0
    for(ix <- 0 until NumShapes){
      // Add k = floor(ideals(ix)) shapes from shapeClasses(ix) to pieces
      val k = ideals(ix).toInt
      for(i <- n until n+k) pieces(i) = choosePiece(ix)
      n += k; remainders(ix) = ideals(ix)-k
    }
    assert(n <= numPieces && numPieces-n < NumShapes)

    // Add numPieces-n extra shapes from classes in extras: those with largest
    // remainders.
    val extras = (0 until NumShapes).sortBy(ix => -remainders(ix))
    for(i <- 0 until numPieces-n) pieces(n+i) = choosePiece(extras(i))
    //for(ix <- extras) pieces ::= choosePiece(ix)

    // Shuffle pieces into nextPieces
    def swap(i: Int, j: Int) = { 
      val t = pieces(i); pieces(i) = pieces(j); pieces(j) = t
    }
    for(i <- 0 until numPieces) swap(i, i+Random.nextInt(numPieces-i))

    // Now try to even out clusters of the same shape: traverse, recording how
    // many of each shape have been seen so far; if a piece is encountered
    // that already exceeds its expected count, then swap it with another
    // piece.
    val counts = new Array[Int](NumShapes)
    for(i <- 0 until numPieces-1){
      val sh = pieces(i).shapeIndex
      if(counts(sh) > i*probs(sh)) swap(i, i+Random.nextInt(numPieces-i))
      counts(pieces(i).shapeIndex) += 1
    }

    nextPieces = pieces.toList
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
    for(x <- 0 until width; y <- 0 until height) grid(x)(y) match{
      case p: Piece =>
        for((dx,dy) <- p.deltas){
          val xx = x+dx; val yy = y+dy
          // There should be a piece at (xx,yy)
          if(xx < 0 || xx >= width ||
            (if(yy < 0) x != sourceX else if(yy >= height) x != sinkX
            else grid(xx)(yy) == null)
          ) return false
        }
      case _ => {}
    } // end of match/for loop
// FIXME: also check connected
    true
  }

  /** Time to fill one square from the source. */
  private val SquareFillTime = 180

  /** Animate the filling of the pipes. */
  private def fillPipes() = {
    import Piece.{S,End,endToDelta,reverse}
    println("End of level")
    frame.setFilling(true)
    /* We perform a breadth-first traversal.  At each ply, we fill all pieces in
     * the current ply, then move to the next ply.  Note, though, that in
     * order to cater for cross-over pieces, we need to keep track of the
     * direction from which a piece was entered.  `frontier` holds the current
     * frontier of the traversal: each coordinate together with the direction
     * from which it was entered.  `seen` holds each coordinate reached
     * previously, together with each end connected to the end by which it was
     * entered. */ 
    var frontier = List((sourceX,0,S))
    grid(sourceX)(0).asPiece.enterFrom(S)
    val seen = Array.ofDim[Boolean](width, height, 4); 
    for(end <- grid(sourceX)(0).asPiece.connectsTo(S))
      seen(sourceX)(0)(end) = true
    // println()
    while(frontier.nonEmpty){ 
      // Animate filling.
      val frontierPs = 
        frontier.map{case(x,y,_) => (x,y)}.distinct.map{case (x,y) => grid(x)(y)}
      for(i <- 0 until FillSteps){
        for(p <- frontierPs) p.asPiece.fillStep()
        Thread.sleep(SquareFillTime*(frontier.length+1)/(2*FillSteps))
        frame.update()
      }
      // Find frontier for next iteration
      var newFrontier = List[(Int,Int,End)]()
      for((x,y,d) <- frontier; end <- grid(x)(y).asPiece.connectsTo(d)){ 
        // We entered from d, so we can exit via end
        val (dx,dy) = endToDelta(end); val xx = x+dx; val yy = y+dy
        if(yy == height) assert((x,y) == (sinkX,height-1))
        else if(yy < 0) assert((x,y) == (sourceX,0))
        else if(!seen(xx)(yy)(reverse(end))){   
          // water can flow from (x,y) into (xx)(yy), entering via reverse(end)
          grid(xx)(yy).asPiece.enterFrom(reverse(end))
          newFrontier ::= ((xx,yy,reverse(end)))
        }
      } // end of for loop
      frontier = newFrontier.distinct
      // Record that we've seen these triples, but also any other connected
      // end of the same square.
      for((xx,yy,e) <- frontier; e1 <- grid(xx)(yy).asPiece.connectsTo(e))
        seen(xx)(yy)(e1) = true
      // Note: we update `seen` only at the end of the ply, because we might
      // want water to enter a particular piece from two directions.
    } // end of while(frontier.nonEmpty)
  }

  /** Play at (x,y). */
  def playAt(x: Int, y: Int) = if(grid(x)(y) == null){
    grid(x)(y) = currentPiece; addScore(currentPiece.score)
    if(isLevelOver) Concurrency.runThread{ 
      fillPipes(); Thread.sleep(1500); nextLevel() 
    }
    else getNextPiece()
  }

  /** Kill the piece at (x,y). */
  def killAt(x: Int, y: Int) = { 
    println("kill")
    if((x,y) == sink) println("Sink")
    else if((x,y) == source) println("Source")
  // FIXME: not source, sink
    else if(killsLeft > 0) grid(x)(y) match{
      case p: Piece => grid(x)(y) = null; killPiece(p)
      case _ => println("Non-kill")
    }
    else println("No kills left") // IMPROVE
  }

  /** Kill piece p. */
  private def killPiece(p: Piece) = {
    // Apply penalty equal to the piece's value.
    addScore(-p.score)
    // Replace at end of queue
    nextPieces = nextPieces :+ p
    killsLeft -= 1; frame.updateInfo()
  }

  /** Kill the current piece. */
  def killCurrentPiece() = {
    if(killsLeft > 0){
      killPiece(currentPiece)
      // // Apply penalty equal to the piece's value.
      // addScore(-currentPiece.score)
      // // Replace at end of queue
      // nextPieces = nextPieces :+ currentPiece
      // killsLeft -= 1; frame.updateInfo()
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
}
