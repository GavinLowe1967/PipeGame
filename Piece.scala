package pipegame


object Piece{
  /** Type representing an open end of a piece. */
  type End = Int

  /* Representation of an open end of a piece. */
  val N = 0; val S = 1; val E = 2; val W = 3

  /** The change in coordinates corresponding to moving in the direction given
    * by `end`. */
  def endToDelta(end: End) = end match{ 
    case N => (0,1); case S => (0,-1); case E => (1,0); case W => (-1,0)
  }

  /** The reverse of the direction given by end. */
  def reverse(end: End) = end match{
    case N => S; case S => N; case E => W; case W => E 
  }

  /** The number of different shapes.  Each `shapeIndex` is in the range
    * [0 .. NumShapes). */
  val NumShapes = 5

  /** The different pieces, partitioned by shape. */
  val shapeClasses: Array[Array[() => Piece]] = Array(
    Array(NS, EW), Array(NE, NW, SE, SW), Array(NES, ESW, SWN, WNE),
    Array(Cross), Array(NSOverEW, EWOverNS)
  )

  for(i <- 0 until NumShapes) assert(shapeClasses(i).forall(_().shapeIndex == i))

  /** The number of steps taken to fill a piece during the fill animation. */
  val FillSteps = 10
}

import Piece._

// =======================================================

/** A piece on the board. */
trait Piece{
  /** The piece obtained by rotating this left through 90 degrees. */
  def rotateLeft: Piece

  /** The piece obtained by rotating this right through 90 degrees. */
  def rotateRight: Piece

  /** The list of open ends of a piece. */
  val ends: List[End]

  /** The offsets to squares attached to this piece by open ends. */
  def deltas: List[(Int,Int)] = ends.map(endToDelta)

  /** An index giving the shape of the piece: 0 = straight; 1 = curve; 2 = T; 
    * 3 = cross; 4 = cross-over. */
  val shapeIndex: Int

  /** The score for playing a piece. */
  val score: Int

  /** The ends connected to `end`.  By default, all  ends; but this is
    * overwritten for cross-over pieces.  It is assumed that the resulting
    * relation is symmetric. */
  def connectsTo(end: End): List[End] = ends // .filter(_ != end)

  /** From which ends is water entering? */
  protected val fillingFrom = new Array[Boolean](4)

  /** Water starts to enter from `end`. */
  def enterFrom(end: End) = {
    assert(ends.contains(end)); fillingFrom(end) = true
  }

  /* Each subclass will have variables indicating how much the piece has been
   * filled from each end.  These variables will hold values in the range
   * [0..FillSteps]. */

  /** Proportion of the pipe that has been filled from each end.  Each entry
   * holds a value in the range [0..FillSteps]. */
  val filledFrom: Array[Int]

  /** One unit of water enters.  Expected to be called FillSteps times. */
  def fillStep() = {
    assert(fillingFrom.exists(b => b))
    for(ix <- 0 until ends.length; if fillingFrom(ends(ix))){
      filledFrom(ix) += 1; assert(filledFrom(ix) <= FillSteps)
      if(filledFrom(ix) == FillSteps) fillingFrom(ends(ix)) = false // full
    }
  }
}

// =======================================================

/** Trait for pieces with two ends. */
trait TwoEndPiece extends Piece{
  /** Proportion of the pipe that has been filled from each end. */
  val filledFrom = new Array[Int](2)
}

/* Note: we will follow the convention that, for two-ended pieces, the name of
 * the class matches the order of ends. */

// =======================================================

/** Trait for straight pieces. */
trait StraightPiece extends TwoEndPiece{
  val shapeIndex = 0
  val score = 1
}

/** A pipe running North-South. */
case class NS() extends StraightPiece{
  def rotateLeft = EW()
  def rotateRight = EW()
  val ends = List(N,S)
}

/** A pipe running East-West. */
case class EW() extends StraightPiece{
  def rotateLeft = NS()
  def rotateRight = NS()
  val ends = List(E,W)
}

// ===== Curves

/** Trait for curved pieces. */
trait CurvedPiece extends TwoEndPiece{
  val shapeIndex = 1
  val score = 2
}

/** A curved piece connecting North and East edges. */
case class NE() extends CurvedPiece{
  def rotateLeft = NW()
  def rotateRight = SE()
  val ends = List(N,E)
}

/** A curved piece connecting North and West edges. */
case class NW() extends CurvedPiece{
  def rotateLeft = SW()
  def rotateRight = NE()
  val ends = List(N,W)
}

/** A curved piece connecting South and East edges. */
case class SE() extends CurvedPiece{
  def rotateLeft = NE()
  def rotateRight = SW()
  val ends = List(S,E)
}

/** A curved piece connecting South and West edges. */
case class SW() extends CurvedPiece{
  def rotateLeft = SE()
  def rotateRight = NW()
  val ends = List(S,W)
}

// ===== T-junctions

/** Trait for T-junction pieces. */
trait TJunctionPiece extends Piece{
  val shapeIndex = 2
  val score = 3

  /** Proportion of the pipe that has been filled from each end. */
  val filledFrom = new Array[Int](3)

  // /** Amount by which the piece is filled from the centre. */
  // var filledFromCentre = 0
}

/* Note: we will follow the convention that, for T-junction pieces, the name
 * of the class matches the order of ends. */


/** A T-junction, with pipes to the North, East and South. */
case class NES() extends TJunctionPiece{
  def rotateLeft = WNE()
  def rotateRight = ESW()
  val ends = List(N,E,S)
}

/** A T-junction, with pipes to the East, South and West. */
case class ESW() extends TJunctionPiece{
  def rotateLeft = NES()
  def rotateRight = SWN()
  val ends = List(E,S,W)
}

/** A T-junction, with pipes to the South, West and North. */
case class SWN() extends TJunctionPiece{
  def rotateLeft = ESW()
  def rotateRight = WNE()
  val ends = List(S,W,N)
}

/** A T-junction, with pipes to the West, North and East. */
case class WNE() extends TJunctionPiece{
  def rotateLeft = SWN()
  def rotateRight = NES()
  val ends = List(W,N,E)
}

// ===== Crosses

/** A cross piece, with pipes in all four directions. */
case class Cross() extends Piece{
  def rotateLeft = this
  def rotateRight = this
  val ends = List(N,S,E,W)
  val shapeIndex = 3
  val score = 4
  val filledFrom = new Array[Int](4)
}

// ===== Cross-overs

/** Trait for cross-over pieces. */
trait CrossOverPiece extends Piece{
  val shapeIndex = 4
  val score = 4
  val ends = List(N,S,E,W)
  // The ends attached to the "over" part
  val overEnds: List[End]
  // Each end connects to the opposite end. 
  override def connectsTo(end: End) = List(end, reverse(end))
  val filledFrom = new Array[Int](4)

  // override def fillStep() = {
  //   assert(fillingFrom.exists(b => b))
  //   for(ix <- 0 until ends.length; if fillingFrom(ends(ix))){
  //     println("Filling from "+ends(ix))
  //     filledFrom(ix) += 1; assert(filledFrom(ix) <= FillSteps)
  //     if(filledFrom(ix) == FillSteps) fillingFrom(ends(ix)) = false // full
  //   }
  // }
}

/** A cross-over piece, with NS diretion above EW. */
case class NSOverEW() extends CrossOverPiece{
  def rotateLeft = EWOverNS()
  def rotateRight = EWOverNS()
  val overEnds = List(N,S)
}

/** A cross-over piece, with EW diretion above NS. */
case class EWOverNS() extends CrossOverPiece{
  def rotateLeft = NSOverEW()
  def rotateRight = NSOverEW()
  val overEnds = List(E,W)
}

