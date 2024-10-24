package pipegame

/** A piece on the board. */
trait Piece{
  /** The piece obtained by rotating this left through 90 degrees. */
  def rotateLeft: Piece

  /** The piece obtained by rotating this right through 90 degrees. */
  def rotateRight: Piece

  /** The offsets to squares attached to this piece by open ends. */
  val deltas: List[(Int,Int)]
}

object Piece{
  /* Values to use as offsets in deltas values. */
  val N = (0,1); val S = (0,-1); val E = (1,0); val W = (-1,0)
}

import Piece._

/** A pipe running North-South. */
case object NS extends Piece{
  def rotateLeft = EW
  def rotateRight = EW
  val deltas = List(N,S)
}

/** A pipe running East-West. */
case object EW extends Piece{
  def rotateLeft = NS
  def rotateRight = NS
  val deltas = List(E,W)
}

/** A curved piece connecting North and East edges. */
case object NE extends Piece{
  def rotateLeft = NW
  def rotateRight = SE
  val deltas = List(N,E)
}

/** A curved piece connecting North and West edges. */
case object NW extends Piece{
  def rotateLeft = SW
  def rotateRight = NE
  val deltas = List(N,W)
}

/** A curved piece connecting South and East edges. */
case object SE extends Piece{
  def rotateLeft = NE
  def rotateRight = SW
  val deltas = List(S,E)
}

/** A curved piece connecting South and West edges. */
case object SW extends Piece{
  def rotateLeft = SE
  def rotateRight = NW
  val deltas = List(S,W)
}

/** A T-junction, with pipes to the North, East and South. */
case object NES extends Piece{
  def rotateLeft = WNE
  def rotateRight = ESW 
  val deltas = List(N,E,S)
}

/** A T-junction, with pipes to the East, South and West. */
case object ESW extends Piece{
  def rotateLeft = NES
  def rotateRight = SWN
  val deltas = List(E,S,W)
}

/** A T-junction, with pipes to the South, West and North. */
case object SWN extends Piece{
  def rotateLeft = ESW
  def rotateRight = WNE
  val deltas = List(S,W,N)
}


/** A T-junction, with pipes to the West, North and East. */
case object WNE extends Piece{
  def rotateLeft = SWN
  def rotateRight = NES
  val deltas = List(W,N,E)
}

/** A cross piece, with pipes in all four directions. */
case object Cross extends Piece{
  def rotateLeft = Cross
  def rotateRight = Cross
  val deltas = List(N,S,E,W)
}

/** A cross-over piece, with NS diretion above EW. */
case object NSOverEW extends Piece{
  def rotateLeft = EWOverNS
  def rotateRight = EWOverNS
  val deltas = List(N,S,E,W)
}

/** A cross-over piece, with EW diretion above NS. */
case object EWOverNS extends Piece{
  def rotateLeft = NSOverEW
  def rotateRight = NSOverEW
  val deltas = List(N,S,E,W)
}

