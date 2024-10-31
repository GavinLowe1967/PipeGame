package pipegame

import scala.util.Random

class Model(val width: Int, val height: Int){
  //import Model.{scoreForPiece}
  import Piece.NumShapes

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

  /** The maximum number of pieces in play. */
  val maxPieces = size+NumNextPieces

  /** The next pieces.  */
  private var nextPieces: List[Piece] = List[Piece]()

  @volatile private var currentPiece: Piece = null

  /** Get the current piece.  Called by the PipePanel and TopPanel. */
  def getCurrentPiece = currentPiece 

  /** The current level. */
  private var level = 0

  /** Get the current level.  Called by InfoPanel. */
  def getLevel = level

  /** Count of how many pieces of each shape have been filled in the current
    * level. */
  // private val shapeCount = new Array[Int](NumShapes)

  /** Number of pieces played. */
  //private var count = 0

  /** Adjusted probability for choosing a piece with shapeIndex index, based on
    * the squares so far, and that we are aiming for a proportion of p
    * overall. */
//   private def adjustProb(p: Double, index: Int) = {
//     /* We would expect to play p*maxPieces of these in a fully filled grid
//      * (including pieces in the queue); so another
//      * p*maxPieces-shapeCount(index) in the remaining maxPieces-count squares
//      * and queue slots.  */
//     ( (p*maxPieces-shapeCount(index)) / (maxPieces-count).toDouble ) //max 0
//     // The "max 0" can make the probabilities not sum to 1.
// // *** think about this
//   }

  /** Pick a Piece at random. */
  private def choosePiece(): Piece = {
    val p = nextPieces.head; nextPieces = nextPieces.tail; p
  }
/*
    import LevelInfo._; import Piece.{shapeClasses,NumShapes}
    /* We pick based on probabilities from levels(level-1), or defaultLevel. */
    val LevelInfo(probs) = 
      if(level <= levels.length) levels(level-1) else defaultLevel
    // Adjust the probabilities based on the numbers of shapes played so far.
    val adjProbs = Array.tabulate(NumShapes)(i => adjustProb(probs(i), i))
    assert(Math.abs(adjProbs.sum-1) < 0.0001,
      probs.mkString(", ")+"\n"+shapeCount.mkString(", ")+"\n"+
        adjProbs.mkString(", "))
    // Find min i s.t. sum adjProbs[0..i] >= rand
    var i = 0; var sum = 0.0; val rand = Random.nextFloat(); var done = false
    while(!done){ sum += adjProbs(i); if(sum >= rand) done = true else i += 1 }
    // Choose from shapeClasses(i)
    val ps = shapeClasses(i); ps(Random.nextInt(ps.length))()
  }
 */

  // /** Pick a Piece at random, and record it in shapeCount and count. */
  // private def choosePieceAndTally(): Piece = {
  //   val p = choosePiece(); shapeCount(p.shapeIndex) += 1; count += 1; p
  // }

  /** Initialise the next level. */
  private def initLevel() = {
    import Piece.{N,S,E,W}
    level += 1; // count = 0
    // for(i <- 0 until NumShapes) shapeCount(i) = 0
    for(x <- 0 until width; y <- 0 until height) grid(x)(y) = null 
    initNextPieces()
    // Choose source and sink pieces and positions, ensuring level is feasible
    sourceX = Random.nextInt(width); var p = choosePiece()
    while(!p.ends.contains(S) || (sourceX == 0 && p.ends.contains(W)) ||
        (sourceX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = choosePiece()
      // If we can't place p here, we add it to the end of the queue; but we
      // rotate it to guard against the case that no piece can be placed here.
    }
    grid(sourceX)(0) = p; // shapeCount(p.shapeIndex) += 1; count += 1
    sinkX = Random.nextInt(width); p = choosePiece()
    while(!p.ends.contains(N) || (sinkX == 0 && p.ends.contains(W)) ||
        (sinkX == width-1 && p.ends.contains(E))){
      nextPieces = nextPieces :+ p.rotateLeft; p = choosePiece()
    }
    grid(sinkX)(height-1) = p; // shapeCount(p.shapeIndex) += 1; count += 1
    currentPiece = choosePiece()
    println(nextPieces.mkString(", "))
    // nextPieces = 
    //   (for(_ <- 0 until NumNextPieces) yield choosePieceAndTally()).toList
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
    println(ideals.map(_.toString).mkString(", "))
    val remainders = new Array[Double](NumShapes) 
    // `remainders` will hold the fractional parts of `ideals`.  `pieces` are
    // the pieces chosen so far.  `len = pieces.length`.
    var pieces = List[Piece](); var len = 0
    for(ix <- 0 until NumShapes){
      // Add k = floor(ideals(ix)) shapes from shapeClasses(ix) to nextPieces.
      val k = ideals(ix).toInt; len += k; remainders(ix) = ideals(ix)-k
      pieces = List.fill(k)(choosePiece(ix)) ++ pieces
    }
    assert(len <= size && size-len < NumShapes)
    // Add extra shapes from classes in extras: those with largest remainders.
    val extras = (0 until NumShapes).sortBy(ix => -remainders(ix)).take(size-len)
    for(ix <- extras){
      val p = choosePiece(ix); println(s"Adding $p"); pieces ::= p //choosePiece(ix)
    }
    // Shuffle 
    nextPieces = List(); len = size // Inv: len = pieces.length
    while(len > 0){
      val k = random.nextInt(len); val (pref, p::suf) = pieces.splitAt(k)
      nextPieces ::= p; pieces = pref++suf; len -= 1
    }
    assert(pieces.isEmpty); println(nextPieces.mkString(", "))
  }

  /** Choose a random piece from shapeClasses(ix). */
  @inline private def choosePiece(ix: Int): Piece = {
    val ps = Piece.shapeClasses(ix); ps(random.nextInt(ps.length))()
  }

  /** Update state to move to the next piece. */
  private def getNextPiece() = {
    currentPiece = nextPieces.head; 
    nextPieces = nextPieces.tail :+ choosePiece() // AndTally()
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

  /** Play at (x,y). */
  def playAt(x: Int, y: Int) = if(grid(x)(y) == null){
    grid(x)(y) = currentPiece; addScore(currentPiece.score)
    if(isLevelOver){ println("End of level"); Thread.sleep(500); initLevel() }
    else getNextPiece()
  }

  /** Kill the current piece. */
  def killPiece() = {
    if(killsLeft > 0){
      // Apply penalty equal to the piece's value.
      addScore(-currentPiece.score)
      // Replace at end of queue
      nextPieces = nextPieces :+ currentPiece
      // shapeCount(currentPiece.shapeIndex) -= 1; count -= 1
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
  private val random = 
    new Random(Random.nextInt() ^ java.lang.System.currentTimeMillis)
}
