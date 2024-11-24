package pipegame

import scala.util.Random

/** Information about a level.
  * 
  * Each probs(i) gives the probability of choosing a shape from
  * Piece.shapeClasses(i). */ 
case class LevelInfo(probs: Array[Double], numObstacles: Int){
  // Check probabilities sum to 1 (modulo rounding).
  assert(Math.abs(probs.sum-1) < 0.0001)

  override def toString = s"LevelInfo(${probs.mkString(", ")}, $numObstacles)"
}


object LevelInfo{
  /** Information about the levels of the game. */
  private val levels = Array(
    null,  // levels are indexed from 0, so a dummy value
    LevelInfo(Array(0.50, 0.50, 0.0, 0.0, 0.0), 3),  // straights & curves
    LevelInfo(Array(0.30, 0.70, 0.0, 0.0, 0.0), 3),  // straights & curves
    // LevelInfo(0.6, 0.4, 0.0, 0.0, 0.0),
    // LevelInfo(0.4, 0.6, 0.0, 0.0, 0.0),
    LevelInfo(Array(0.30, 0.62, 0.0, 0.04, 0.04), 3), // crosses, cross-overs
    LevelInfo(Array(0.26, 0.58, 0.0, 0.08, 0.08), 3), // crosses, cross-overs
    LevelInfo(Array(0.32, 0.52, 0.0, 0.08, 0.08), 3),
    // LevelInfo(Array(0.42, 0.42, 0.0, 0.08, 0.08)), // too hard!
    LevelInfo(Array(0.40, 0.40, 0.20, 0.0, 0.0), 3),  // T-pieces
    LevelInfo(Array(0.30, 0.50, 0.20, 0.0, 0.0), 3),  // T-pieces
    LevelInfo(Array(0.35, 0.45, 0.0, 0.10, 0.10), 3),
    LevelInfo(Array(0.33, 0.43, 0.0, 0.12, 0.12), 3)
  )

  private val defaultLevel =
    LevelInfo(Array(0.4, 0.39, 0.15, 0.03, 0.03), 3)

  private def getStraightCurveLevel(level: Int) = {
    println(level)
    if(Random.nextInt(2) == 0){
      // Mostly straights.  At most level/2 obstacles.  Proportion of
      // straights between 60% and 88%.  Normally (proportion of
      // straights-60)+2*obstacles = level
      val numObs = Random.nextInt(level/2)+5 // FIXME
      val pSt = ((level-2*numObs+60) min 88).toDouble / 100
// IMPROVE: pick pSt first
      // val seed = Random.nextInt(level) min 34
      // val sts: Double = (60+seed).toDouble / 100.0
      LevelInfo(Array(pSt, 1.0-pSt, 0.0, 0.0, 0.0), numObs)
    }
    else{ 
      // Mostly curves.  At most level/2 obstacles.  Between 6% and 40%
      // straights.  Normally (40-proportion of straights)/2 + 2*obstacles =
      // level
      val numObs =  Random.nextInt(level/2)+5 // FIXME
      val pSt = (40-2*(level-2*numObs) max 6).toDouble / 100
      LevelInfo(Array(pSt, 1.0-pSt, 0.0, 0.0, 0.0), numObs)
      // val seed = Random.nextInt(level) min 17
      // val sts = (40-2*seed).toDouble / 100
      // LevelInfo(Array(sts, 1.0-sts, 0.0, 0.0, 0.0), level-seed)
    }
  }

  /** Get a LevelInfo with a difficulty level corresponding to `level`.  Note:
    * `level` is based on the game level and the difficulty adjustment.  */
  def get(level: Int): LevelInfo = 
    if(true) getStraightCurveLevel(level)
    else if(level < levels.length) levels(level) 
    else defaultLevel

}
