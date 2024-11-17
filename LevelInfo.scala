package pipegame

import scala.util.Random

/** Information about a level.
  * 
  * Each probs(i) gives the probability of choosing a shape from
  * Piece.shapeClasses(i). */ 
case class LevelInfo(probs: Array[Double]){
  // Check probabilities sum to 1 (modulo rounding).
  assert(Math.abs(probs.sum-1) < 0.0001)

  override def toString = s"LevelInfo(${probs.mkString(", ")})"
}


object LevelInfo{
  /** Information about the levels of the game. */
  private val levels = Array(
    null,  // levels are indexed from 0, so a dummy value
    LevelInfo(Array(0.50, 0.50, 0.0, 0.0, 0.0)),  // straights & curves
    LevelInfo(Array(0.30, 0.70, 0.0, 0.0, 0.0)),  // straights & curves
    // LevelInfo(0.6, 0.4, 0.0, 0.0, 0.0),
    // LevelInfo(0.4, 0.6, 0.0, 0.0, 0.0),
    LevelInfo(Array(0.30, 0.62, 0.0, 0.04, 0.04)), // crosses, cross-overs
    LevelInfo(Array(0.26, 0.58, 0.0, 0.08, 0.08)), // crosses, cross-overs
    LevelInfo(Array(0.32, 0.52, 0.0, 0.08, 0.08)),
    // LevelInfo(Array(0.42, 0.42, 0.0, 0.08, 0.08)), // too hard!
    LevelInfo(Array(0.40, 0.40, 0.20, 0.0, 0.0)),  // T-pieces
    LevelInfo(Array(0.30, 0.50, 0.20, 0.0, 0.0)),  // T-pieces
    LevelInfo(Array(0.35, 0.45, 0.0, 0.10, 0.10)),
    LevelInfo(Array(0.33, 0.43, 0.0, 0.12, 0.12))
  )

  private val defaultLevel =
    LevelInfo(Array(0.4, 0.39, 0.15, 0.03, 0.03))

  private def getStraightCurveLevel(level: Int) = {
    // Proportion of pieces that are straight
    val sts: Double = 
      (if(Random.nextInt(2) == 0) 60+level min 94 else 40-2*level max 6) / 100.0
    LevelInfo(Array(sts, 1.0-sts, 0.0, 0.0, 0.0))
  }

  /** Get a LevelInfo with a difficulty level corresponding to `level`.  Note:
    * `level` is based on the game level and the difficulty adjustment.  */
  def get(level: Int): LevelInfo = 
    if(true) getStraightCurveLevel(level)
    else if(level < levels.length) levels(level) 
    else defaultLevel

}
