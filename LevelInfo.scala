package pipegame

/** Information about a level.
  * 
  * Each probs(i) gives the probability of choosing a shape from
  * Piece.shapeClasses(i). */ 
case class LevelInfo(probs: Array[Double]){
  // Check probabilities sum to 1 (modulo rounding).
  assert(Math.abs(probs.sum-1) < 0.0001)
}


object LevelInfo{

  val levels = Array(
    LevelInfo(Array(0.50, 0.50, 0.0, 0.0, 0.0)),
    // LevelInfo(0.6, 0.4, 0.0, 0.0, 0.0),
    // LevelInfo(0.4, 0.6, 0.0, 0.0, 0.0),
    LevelInfo(Array(0.22, 0.62, 0.0, 0.08, 0.08)),
    LevelInfo(Array(0.32, 0.52, 0.0, 0.08, 0.08)),
    // LevelInfo(Array(0.42, 0.42, 0.0, 0.08, 0.08)), // too hard!
    LevelInfo(Array(0.40, 0.40, 0.20, 0.0, 0.0)),
    LevelInfo(Array(0.35, 0.45, 0.0, 0.10, 0.10)),
    LevelInfo(Array(0.33, 0.43, 0.0, 0.12, 0.12))
  )

  val defaultLevel =
    LevelInfo(Array(0.4, 0.39, 0.15, 0.03, 0.03))

}
