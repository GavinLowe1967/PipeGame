package pipegame

import scala.util.Random

class Model(val width: Int, val height: Int){

  /** The frame.  Set by the MazeGame object. */
  private var frame: FrameT = null

  /** Set the frame to be f, and initialise the level. */
  def setFrame(f: FrameT) = { 
    frame = f; initLevel()
  }

  val random = new Random(Random.nextInt() ^ java.lang.System.currentTimeMillis)

  /** The grid. */
  val grid = Array.ofDim[Piece](width, height)

  /* The source and sink of the flow. */
  private val source = (0,-1)
  private val sink = (width-1,height)

  /** The number of next pieces in the queue. */
  val NumNextPieces = width

  /** The next pieces.  */
  private var nextPieces: List[Piece] = List[Piece]()

  /** The next piece. */
  def nextPiece = synchronized{ nextPieces.head }

  /* Probability of a Cross piece, a T-piece, respectively, a straight piece. */
  private val XProb = 0.03; private val CrossOverProb = 0.03
  private val TProb = 0.15; private val StraightProb = 0.40
  // Note: above definitions must precede initialisation of pieces.

  private def initLevel() = {
    for(x <- 0 until width; y <- 0 until height) grid(x)(y) = null 
    grid(0)(0) = NS; grid(width-1)(height-1) = NS
    // TODO: sink, source
    nextPieces = List[Piece]()
    for(_ <- 0 until NumNextPieces) nextPieces ::= choosePiece()
    frame.setNextPieces(nextPieces)
  }


  /** Pick a Piece at random. */
  private def choosePiece(): Piece = {
    val rand = random.nextFloat()
    if(rand <= XProb) Cross
    else if(rand <= XProb+CrossOverProb) // Cross-over piece
      random.nextInt(2) match{ case 0 => NSOverEW; case 1 => EWOverNS }
    else if(rand <= XProb+CrossOverProb+TProb) // T-piece
      random.nextInt(4) match{ 
        case 0 => NES; case 1 => ESW; case 2 => SWN; case 3 => WNE
      }
    else if(rand <= XProb+CrossOverProb+TProb+StraightProb)  // straight piece
      random.nextInt(2) match{ case 0 => NS; case 1 => EW }
    else // bend piece 
      random.nextInt(4) match{
        case 0 => NE; case 1 => NW; case 2 => SE; case 3 => SW
      }
  }

  /** Update state to move to the next piece. */
  private def getNextPiece() = {
    synchronized{ nextPieces = nextPieces.tail :+ choosePiece() } 
    frame.setNextPieces(nextPieces)
  }

  /** The current score. */
  private var score = 0

  /** Add s to the score. */
  private def addScore(s: Int) = { score += s; frame.setScore(score) }

  /** The score for playing piece p. */
  private def scoreForPiece(p: Piece) = p match{
    case NS | EW => 1                // straight piece
    case NE | NW | SE | SW => 2      // bend piece
    case NES | ESW | SWN | WNE => 3  // T-piece
    case Cross => 4                  // cross piece
    case NSOverEW | EWOverNS => 4    // cross-over piece
  }

  /** Play at (x,y). */
  def playAt(x: Int, y: Int) = if(grid(x)(y) == null){
    val p = nextPiece; grid(x)(y) = p; addScore(scoreForPiece(p))
    getNextPiece()
  }

  /** Kill the current piece. */
  def killPiece() = {
    // Apply penalty equal to the piece's value.
    addScore(-scoreForPiece(nextPiece))
    getNextPiece()
  }

  /** Replace the first piece by p. */
  private def changeFirstPiece(p: Piece) = {
    synchronized{ nextPieces = p :: nextPieces.tail}
    frame.setNextPieces(nextPieces)
  }

  /** Rotate the next piece left. */
  def rotateLeft() = changeFirstPiece(nextPiece.rotateLeft)

  /** Rotate the next piece right. */
  def rotateRight() = changeFirstPiece(nextPiece.rotateRight)

  def endLevel() = { 
    println("endLevel") 
    // Test if every pipe end is connected
    for(x <- 0 until width; y <- 0 until height){
      val p = grid(x)(y)
      if(p != null) for((dx,dy) <- p.deltas){
        val xx = x+dx; val yy = y+dy
        // There should be a piece at (xx,yy)
        if((xx,yy) != sink && (xx,yy) != source && (
            xx < 0 || xx >= width || yy < 0 || yy >= height ||
            grid(xx)(yy) == null
        )){
          println(s"Missing piece at ($xx,$yy) adjacent to ($x,$y)")
          // ...
        }
      }

        // TODO: also check connected
    }

    // New level
    Thread.sleep(1000); initLevel()

  }


}
