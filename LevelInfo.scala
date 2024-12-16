package pipegame

import scala.util.Random

/** Information about a level.
  * 
  * Each probs(i) gives the probability of choosing a shape from
  * Piece.shapeClasses(i). */ 
case class LevelInfo(probs: Array[Double], numObstacles: Int){
  // Check probabilities sum to 1 (modulo rounding).
  assert(Math.abs(probs.sum-1) < 0.0001, toString)

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

// FIXME: absolute upper bound of size-12 obstacles

  private def getStraightCurveLevel(level: Int) = {
    // println(level)
    // Below, pcSt is the percentage of pieces that are straight.  This is
    // chosen in the range [pcStMin..pcStMax].  numObs is the number of
    // obstructions.
    if(Random.nextInt(2) == 0){ 
      // Include mostly straights. 
      val pcStMin = 58; val pcStMax = 75; val pcStRange = pcStMax-pcStMin
      // We aim for pSt-pStMin + 2*numObs = level, and pSt in [pStMin..pStMax].
      // So numObs = (level-(pSt-pStMin)/2,
      // so (level-pStRange)/2 max 0 <= numObs <= level/2 
      val numObs = 
        if(level <= pcStRange) Random.nextInt(level/2+1) 
        else (level - Random.nextInt(pcStRange+1)) / 2
      val pcSt = level-2*numObs+pcStMin; val pStd = pcSt/100.0
      LevelInfo(Array(pStd, 1.0-pStd, 0.0, 0.0, 0.0), numObs)
    }
    else{ 
      // Mostly curves.  
      val pcStMin = 15; val pcStMax = 40; val pcStRange = pcStMax-pcStMin
      // We aim for (pcStMax-pcSt)/2 + 2*numObs = level.
      // So numObs = level/2 - (psCstMax-pcSt)/4.
      // So (level - pcStRange/2)/2 max 0  <= numObs <= level/2
      val numObs = 
        if(level <= pcStRange/2) Random.nextInt(level/2+1)
        else (level - Random.nextInt(pcStRange/2+1))/2  
      val pcSt = pcStMax-2*level+4*numObs; val pSt = pcSt/100.0
      LevelInfo(Array(pSt, 1.0-pSt, 0.0, 0.0, 0.0), numObs)
    }
  }

// IMPROVE: allow at most 50% obstacles

  /** Get a LevelInfo for a level using crosses, straights and curves. */
  private def getCrossLevel(level: Int) = {
    // println(level)
    // At present, don't adapt number of straights based on level.  The
    // proportion of non-crosses will be in the following ratio.
    val pSt0 = 0.3; val pCurve0 = 1.0-pSt0
    val XCost = 4; val ObsCost = 2 // costs of cross(overs), obstacles
    val MaxPcX = 50; val MaxObs = 32 // max % cross(overs), # obstacles
    // Aim for XCost*pcX + ObsCost*numObs = level
    val minPcX = (level-ObsCost*MaxObs)/XCost max 0 // min % cross(overs)
    val pcX = // percentage of pieces that are crosses/crossovers 
      minPcX + Random.nextInt(level/XCost-minPcX+1) min 50
    val numObs = ((level-XCost*pcX)/ObsCost).toInt max 0 // # obstacles
    val pcCross = Random.nextInt(pcX+1)
      // if(pcCrossCrossOver <= 1) pcCrossCrossOver
      // else pcCrossCrossOver*Random.nextDouble() // % of crosses
    val pCross = pcCross/100.0  // proportion of crosses
    val pCrossOver = (pcX-pcCross)/100.0 // proportion of crossovers
    val pStCurve = 1-pcX/100.0 // prop straights, curves
    val pSt = pSt0*pStCurve; val pCurve = pCurve0*pStCurve // props st, curves
    LevelInfo(Array(pSt, pCurve, 0.0, pCross, pCrossOver), numObs)
  }

  /** Get a level using T-pieces, straighs and curves. */
  private def getTLevel(level: Int) = {
    // At present, don't adapt number of straights based on level.  The
    // proportion of non-crosses will be in the following ratio.
    val pSt0 = 0.3; val pCurve0 = 1.0-pSt0
    val TCost = 4; val ObsCost = 2 // cost of Ts, obstacles
    val MinPcT = 15 // minimum % of T pieces
    // Aim for TCost*(pcT-MinPcT) + ObsCost*numObs = level
    val pcT1 = Random.nextInt(level/TCost+1)
    val pcT = MinPcT+pcT1 // % of T pieces
    val numObs = (level-pcT1*TCost)/ObsCost // # obstacles
    val pT = pcT/100.0 // proportion of Ts
    val pStCurve = 1.0-pT // proportions straights & curves
    val pSt = pStCurve*pSt0; val pCurve = pStCurve*pCurve0
    LevelInfo(Array(pSt, pCurve, pT, 0.0, 0.0), numObs)
  }


  private val gradient = 2

  private var adjustment = 0

  def init(adj: Int) = adjustment = adj

  /** Get a LevelInfo with a difficulty level corresponding to `gameLevel`.
    * Note: `level` is based on the game level and the difficulty
    * adjustment.  */
  def get(gameLevel: Int): LevelInfo = {
    // `level` represents a level of difficulty based on `gameLevel`. 
    val level = gameLevel*gradient+adjustment
    println(s"$gameLevel -> $level") 
    if(true) Random.nextInt(3) match{
// IMPROVE: beyond about 50, getStraightCurveLevel becomes v.diff
      case 0 if false && level <= 100 => getStraightCurveLevel(level) 
      case 1 if false => getCrossLevel(level)
// FIXME
      case 2 => getTLevel(level)
      case _ => get(gameLevel) // retry
    }
    else if(level < levels.length) levels(level) 
    else defaultLevel
  }
}
