package pipegame


object Piece{
  /** Type representing an open end of a piece. */
  type End = Int

  /* Representation of an open end of a piece. */
  val N = 0; val S = 1; val E = 2; val W = 3

  /** The number of different shapes.  Each `shapeIndex` is in the range
    * [0 .. NumShapes). */
  val NumShapes = 5

  /** The different pieces, partitioned by shape. */
  val shapeClasses: Array[Array[Piece]] = Array(
    Array(NS, EW), Array(NE, NW, SE, SW), Array(NES, ESW, SWN, WNE),
    Array(Cross), Array(NSOverEW, EWOverNS)
  )

  for(i <- 0 until NumShapes) assert(shapeClasses(i).forall(_.shapeIndex == i))
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
  def deltas: List[(Int,Int)] = ends.map{ 
    case N => (0,1); case S => (0,-1); case E => (1,0); case W => (-1,0)
  }

  /** An index giving the shape of the piece: 0 = straight; 1 = curve; 2 = T; 
    * 3 = cross; 4 = cross-over. */
  val shapeIndex: Int
}
/** A pipe running North-South. */
case object NS extends Piece{
  def rotateLeft = EW
  def rotateRight = EW
  val ends = List(N,S)
  val shapeIndex = 0
}

/** A pipe running East-West. */
case object EW extends Piece{
  def rotateLeft = NS
  def rotateRight = NS
  val ends = List(E,W)
  val shapeIndex = 0
}

/** A curved piece connecting North and East edges. */
case object NE extends Piece{
  def rotateLeft = NW
  def rotateRight = SE
  val ends = List(N,E)
  val shapeIndex = 1
}

/** A curved piece connecting North and West edges. */
case object NW extends Piece{
  def rotateLeft = SW
  def rotateRight = NE
  val ends = List(N,W)
  val shapeIndex = 1
}

/** A curved piece connecting South and East edges. */
case object SE extends Piece{
  def rotateLeft = NE
  def rotateRight = SW
  val ends = List(S,E)
  val shapeIndex = 1
}

/** A curved piece connecting South and West edges. */
case object SW extends Piece{
  def rotateLeft = SE
  def rotateRight = NW
  val ends = List(S,W)
  val shapeIndex = 1
}

/** A T-junction, with pipes to the North, East and South. */
case object NES extends Piece{
  def rotateLeft = WNE
  def rotateRight = ESW 
  val ends = List(N,E,S)
  val shapeIndex = 2
}

/** A T-junction, with pipes to the East, South and West. */
case object ESW extends Piece{
  def rotateLeft = NES
  def rotateRight = SWN
  val ends = List(E,S,W)
  val shapeIndex = 2
}

/** A T-junction, with pipes to the South, West and North. */
case object SWN extends Piece{
  def rotateLeft = ESW
  def rotateRight = WNE
  val ends = List(S,W,N)
  val shapeIndex = 2
}


/** A T-junction, with pipes to the West, North and East. */
case object WNE extends Piece{
  def rotateLeft = SWN
  def rotateRight = NES
  val ends = List(W,N,E)
  val shapeIndex = 2
}

/** A cross piece, with pipes in all four directions. */
case object Cross extends Piece{
  def rotateLeft = Cross
  def rotateRight = Cross
  val ends = List(N,S,E,W)
  val shapeIndex = 3
}

/** A cross-over piece, with NS diretion above EW. */
case object NSOverEW extends Piece{
  def rotateLeft = EWOverNS
  def rotateRight = EWOverNS
  val ends = List(N,S,E,W)
  val shapeIndex = 4
}

/** A cross-over piece, with EW diretion above NS. */
case object EWOverNS extends Piece{
  def rotateLeft = NSOverEW
  def rotateRight = NSOverEW
  val ends = List(N,S,E,W)
  val shapeIndex = 4
}

