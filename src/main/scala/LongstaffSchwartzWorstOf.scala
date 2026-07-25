import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

/**
 * Implementation of Longstaff-Schwartz algorithm for American callable options
 * on the worst-of multiple assets with a down-and-in barrier feature.
 *
 * SUPPORTS TWO PAYOFF TYPES:
 * 1. PERFORMANCE-BASED (S/S_0): max(K - min(S_i/S_i(0)), 0) - MOST COMMON
 * 2. Absolute price: max(K - min(S_i), 0)
 *
 * Toggle with usePerformance flag. For performance-based:
 * - Strike/barrier in percentage (e.g., 1.0 = 100%, 0.70 = 70%)
 * - Natural normalization (performances ~1.0)
 * - Standard in equity-linked structured products
 *
 * This implementation uses LAGUERRE POLYNOMIALS (orthogonal basis) which provides:
 * - Superior numerical stability compared to standard polynomials
 * - No multicollinearity issues in regression
 * - Ability to use higher degrees (5-7) safely
 * - Follows the original Longstaff-Schwartz (1998) paper
 *
 * QUICK START:
 * - LaguerreBasisFunctions: RECOMMENDED (orthogonal, stable, degrees 5-7)
 * - WorstOfPolynomialBasis: For comparison only (use degrees ≤3)
 *
 * To switch basis functions, simply change in the main example:
 *   new LaguerreBasisFunctions(...) ⟷ new WorstOfPolynomialBasis(...)
 */

/**
 * Represents a multi-asset price at a given time point.
 * @param prices Array of asset prices
 * @param initialPrices Optional initial prices for performance calculation
 */
case class AssetPrices(prices: Array[Double], initialPrices: Option[Array[Double]] = None) {
  def worst: Double = prices.min
  def best: Double = prices.max
  def average: Double = prices.sum / prices.length
  def apply(i: Int): Double = prices(i)
  def numAssets: Int = prices.length

  /**
   * Get performances (S_t / S_0) for each asset.
   * Returns prices if no initial prices provided.
   */
  def performances: Array[Double] = initialPrices match {
    case Some(initials) => (prices zip initials).map { case (p, i) => p / i }
    case None => prices
  }

  /**
   * Get worst performance across all assets.
   * For performance-based payoffs, this is the key monitoring variable.
   */
  def worstPerformance: Double = performances.min

  /**
   * Get best performance across all assets.
   */
  def bestPerformance: Double = performances.max

  /**
   * Get performance of specific asset.
   */
  def performance(i: Int): Double = initialPrices match {
    case Some(initials) => prices(i) / initials(i)
    case None => prices(i)
  }
}

/**
 * Represents the complete price path for multiple assets.
 * @param paths Array where paths(timeIndex)(assetIndex) = price
 * @param usePerformance If true, barrier monitoring uses performance (S/S_0) instead of absolute prices
 * @param referencePrices Optional fixing levels (S_0) used for performance calculation. If omitted, the
 *                        first row of the path (paths(0)) is used. This matters for finite-difference
 *                        greeks: when bumping today's spot for a performance-based (S/S_0) payoff we must
 *                        keep the initial fixing fixed, otherwise the bump cancels out of every
 *                        performance and delta/gamma are identically zero.
 */
case class PricePath(
  paths: Array[Array[Double]],
  usePerformance: Boolean = false,
  referencePrices: Option[Array[Double]] = None
) {
  private val initialPrices: Array[Double] = referencePrices.getOrElse(paths(0))

  def pricesAt(timeIndex: Int): AssetPrices = {
    if (usePerformance) {
      AssetPrices(paths(timeIndex), Some(initialPrices))
    } else {
      AssetPrices(paths(timeIndex))
    }
  }

  def numTimeSteps: Int = paths.length
  def numAssets: Int = if (paths.isEmpty) 0 else paths(0).length

  /**
   * Check if barrier has been breached at any point up to timeIndex.
   * If usePerformance=true, barrier is checked against worst performance (S/S_0).
   * If usePerformance=false, barrier is checked against worst absolute price.
   */
  def barrierBreached(barrier: Double, timeIndex: Int): Boolean = {
    (0 to timeIndex).exists { t =>
      val prices = pricesAt(t)
      if (usePerformance) {
        prices.worstPerformance <= barrier
      } else {
        prices.worst <= barrier
      }
    }
  }
}

/**
 * Option specification for worst-of American callable with down-and-in barrier.
 */
case class WorstOfBarrierOption(
  strike: Double,
  barrier: Double,              // Down-and-in barrier level
  callDates: Array[Int],        // Time indices when option can be called
  maturity: Int,                // Maturity time index
  discountFactors: Array[Double] // Discount factors for each time step
) {
  require(barrier < strike, "Barrier must be below strike for down-and-in put")
  require(callDates.forall(t => t >= 0 && t <= maturity), "Call dates must be within [0, maturity]")
  require(callDates.sorted.sameElements(callDates), "Call dates must be sorted")
}

/**
 * Basis functions for regression in Longstaff-Schwartz algorithm.
 * Designed specifically for worst-of options with barriers.
 */
trait BasisFunctions {
  def apply(prices: AssetPrices, barrierBreached: Boolean): Array[Double]
  def dimension: Int
}

/**
 * Weighted Laguerre polynomials: L_n(x) with weight e^(-x).
 * These are orthogonal polynomials used in the original Longstaff-Schwartz paper.
 */
object LaguerrePolynomials {
  /**
   * Compute all Laguerre polynomials up to degree maxDegree at point x.
   * Recursion: L_0(x) = 1, L_1(x) = 1-x,
   * L_{n+1}(x) = [(2n + 1 - x) * L_n(x) - n * L_{n-1}(x)] / (n + 1)
   */
  def evaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val result = Array.ofDim[Double](maxDegree + 1)
    result(0) = 1.0
    if (maxDegree >= 1) result(1) = 1.0 - x

    for (n <- 1 until maxDegree) {
      result(n + 1) = ((2 * n + 1 - x) * result(n) - n * result(n - 1)) / (n + 1)
    }
    result
  }

  /**
   * Weighted Laguerre basis: L_n(x) * e^(-x/2) for numerical stability.
   */
  def weightedEvaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val weight = math.exp(-x / 2.0)
    evaluateAll(maxDegree, x).map(_ * weight)
  }
}

/**
 * Laguerre basis functions for worst-of options with barriers.
 * Uses orthogonal Laguerre polynomials for superior numerical stability.
 *
 * ADVANTAGES over standard polynomials:
 * - No multicollinearity (orthogonal by construction)
 * - Better numerical stability
 * - Can use higher degrees (5-7) safely
 * - Used in original Longstaff-Schwartz (1998) paper
 *
 * IMPORTANT: For performance-based payoffs, set usePerformance=true
 * This will use S/S_0 instead of absolute prices for basis functions.
 */
class LaguerreBasisFunctions(
  numAssets: Int,
  maxDegree: Int = 5,
  includeBarrierInteractions: Boolean = true,
  normalizeToStrike: Boolean = true,
  strikeLevel: Double = 100.0,
  usePerformance: Boolean = false
) extends BasisFunctions with LazyLogging {

  /**
   * Generate basis using weighted Laguerre polynomials.
   *
   * If usePerformance=true: Uses performances (S/S_0) which are naturally ~1.0
   * If usePerformance=false: Uses prices normalized by strike
   */
  def apply(prices: AssetPrices, barrierBreached: Boolean): Array[Double] = {
    require(prices.numAssets == numAssets, s"Expected $numAssets assets, got ${prices.numAssets}")

    val basis = scala.collection.mutable.ArrayBuffer[Double]()
    val barrierIndicator = if (barrierBreached) 1.0 else 0.0

    // Get worst level (performance or price)
    val worst = if (usePerformance) prices.worstPerformance else prices.worst

    // Normalize: for performances, already ~1.0; for prices, divide by strike
    val normalizedWorst = if (usePerformance) {
      worst  // Performances already normalized
    } else {
      if (normalizeToStrike) worst / strikeLevel else worst
    }

    // 1. Weighted Laguerre polynomials of worst-performing asset
    val laguerreWorst = LaguerrePolynomials.weightedEvaluateAll(maxDegree, normalizedWorst)
    basis ++= laguerreWorst

    // 2. Laguerre polynomials of individual assets (lower degree)
    for (i <- 0 until numAssets) {
      val level = if (usePerformance) prices.performance(i) else prices(i)
      val normalizedLevel = if (usePerformance) {
        level  // Already normalized
      } else {
        if (normalizeToStrike) level / strikeLevel else level
      }
      val laguerre = LaguerrePolynomials.weightedEvaluateAll(math.min(2, maxDegree), normalizedLevel)
      basis ++= laguerre
    }

    // 3. Cross terms: L_1(worst) × L_1(asset_i)
    for (i <- 0 until numAssets) {
      val level = if (usePerformance) prices.performance(i) else prices(i)
      val normalizedLevel = if (usePerformance) {
        level
      } else {
        if (normalizeToStrike) level / strikeLevel else level
      }
      val laguerreWorst1 = LaguerrePolynomials.weightedEvaluateAll(1, normalizedWorst)(1)
      val laguerreAsset1 = LaguerrePolynomials.weightedEvaluateAll(1, normalizedLevel)(1)
      basis += laguerreWorst1 * laguerreAsset1
    }

    // 4. Barrier interactions
    if (includeBarrierInteractions) {
      basis += barrierIndicator

      // Barrier × Laguerre polynomials (low degree)
      for (deg <- 0 to math.min(2, maxDegree)) {
        basis += barrierIndicator * laguerreWorst(deg)
      }
    }

    basis.toArray
  }

  def dimension: Int = {
    var dim = maxDegree + 1  // Laguerre of worst
    dim += numAssets * (math.min(2, maxDegree) + 1)  // Laguerre of individuals
    dim += numAssets  // Cross terms

    if (includeBarrierInteractions) {
      dim += 1  // barrier indicator
      dim += math.min(2, maxDegree) + 1  // barrier × Laguerre
    }

    dim
  }
}

/**
 * Polynomial basis functions tailored for worst-of American options with barriers.
 * (Kept for comparison - Laguerre basis is recommended for production use)
 *
 * This includes:
 * - Powers of the worst-performing asset (key for worst-of options)
 * - Powers of individual asset prices
 * - Cross terms between worst and individual assets
 * - Barrier indicator and interactions
 */
class WorstOfPolynomialBasis(
  numAssets: Int,
  maxDegree: Int = 3,
  includeBarrierInteractions: Boolean = true
) extends BasisFunctions with LazyLogging {

  /**
   * Generate basis functions for given asset prices and barrier status.
   *
   * Basis function structure:
   * 1. Constant term
   * 2. Powers of worst: W, W², W³
   * 3. Individual assets: S₁, S₂, ..., Sₙ
   * 4. Squares of individuals: S₁², S₂², ..., Sₙ²
   * 5. Cross terms: W·S₁, W·S₂, ..., W·Sₙ
   * 6. Barrier indicator
   * 7. Barrier interactions: I_barrier·W, I_barrier·W², etc.
   */
  def apply(prices: AssetPrices, barrierBreached: Boolean): Array[Double] = {
    require(prices.numAssets == numAssets, s"Expected $numAssets assets, got ${prices.numAssets}")

    val basis = scala.collection.mutable.ArrayBuffer[Double]()
    val worst = prices.worst
    val barrierIndicator = if (barrierBreached) 1.0 else 0.0

    // 1. Constant term
    basis += 1.0

    // 2. Powers of worst-performing asset (critical for worst-of options)
    for (deg <- 1 to maxDegree) {
      basis += math.pow(worst, deg)
    }

    // 3. Individual asset prices (linear terms)
    for (i <- 0 until numAssets) {
      basis += prices(i)
    }

    // 4. Quadratic terms of individual assets
    if (maxDegree >= 2) {
      for (i <- 0 until numAssets) {
        basis += math.pow(prices(i), 2)
      }
    }

    // 5. Cross terms: worst × individual assets
    for (i <- 0 until numAssets) {
      basis += worst * prices(i)
    }

    // 6. Barrier indicator
    if (includeBarrierInteractions) {
      basis += barrierIndicator

      // 7. Barrier interactions (captures different behavior when barrier is hit)
      for (deg <- 1 to math.min(2, maxDegree)) {
        basis += barrierIndicator * math.pow(worst, deg)
      }

      // Barrier × individual assets
      for (i <- 0 until numAssets) {
        basis += barrierIndicator * prices(i)
      }
    }

    basis.toArray
  }

  def dimension: Int = {
    var dim = 1 // constant
    dim += maxDegree // powers of worst
    dim += numAssets // individual linear
    dim += (if (maxDegree >= 2) numAssets else 0) // individual squares
    dim += numAssets // cross terms worst × individual

    if (includeBarrierInteractions) {
      dim += 1 // barrier indicator
      dim += math.min(2, maxDegree) // barrier × worst^k
      dim += numAssets // barrier × individual
    }

    dim
  }
}

/**
 * Payoff calculator for down-and-in put on worst-of assets.
 *
 * @param strike Strike level (e.g., 1.0 for 100% or 100.0 for absolute)
 * @param barrier Barrier level (e.g., 0.70 for 70% performance or 70.0 for absolute)
 * @param usePerformance If true, payoff based on performance (S/S_0); if false, absolute prices
 */
class DownAndInWorstOfPut(
  strike: Double,
  barrier: Double,
  usePerformance: Boolean = false
) extends LazyLogging {

  /**
   * Calculate payoff at maturity.
   * Down-and-in: only pays if barrier was breached during the life.
   *
   * For performance-based: max(Strike - WorstPerformance, 0) if barrier hit
   * For absolute price: max(Strike - WorstPrice, 0) if barrier hit
   */
  def payoff(path: PricePath, timeIndex: Int): Double = {
    val prices = path.pricesAt(timeIndex)
    val worstLevel = if (usePerformance) prices.worstPerformance else prices.worst
    val breached = path.barrierBreached(barrier, timeIndex)

    if (breached) {
      // Barrier was hit, put is active
      math.max(strike - worstLevel, 0.0)
    } else {
      // Barrier never hit, option knocked out
      0.0
    }
  }

  /**
   * Intrinsic value if exercised early (before maturity).
   * For callable American: the holder's exercise value.
   */
  def intrinsicValue(prices: AssetPrices): Double = {
    val worstLevel = if (usePerformance) prices.worstPerformance else prices.worst
    math.max(strike - worstLevel, 0.0)
  }
}

/**
 * A calibrated exercise strategy: the least-squares continuation-value coefficients
 * for each call date, as produced by [[LongstaffSchwartzPricer.calibrate]].
 *
 * Holding this fixed while re-simulating bumped paths is what lets the finite-difference
 * greeks engine test the "frozen strategy vs. recalibrate-per-bump" question: the
 * regression coefficients ARE the exercise boundary, so freezing them removes the
 * regression noise that otherwise differs between the up- and down-bump scenarios.
 *
 * @param coefficients Map from call-date time index to its regression coefficient vector.
 *                     A call date is absent if there were too few in-the-money paths to
 *                     regress on during calibration (no early-exercise decision is made there).
 */
case class ExerciseStrategy(coefficients: Map[Int, Array[Double]])

/**
 * Longstaff-Schwartz algorithm implementation for pricing American options.
 */
class LongstaffSchwartzPricer(
  option: WorstOfBarrierOption,
  basisFunctions: BasisFunctions,
  payoffCalculator: DownAndInWorstOfPut
) extends LazyLogging {

  /**
   * Price the option using simulated paths.
   *
   * This is the standard in-sample estimate: the exercise strategy is calibrated on
   * `paths` and immediately applied to the same `paths`. Equivalent to
   * `priceWithStrategy(paths, calibrate(paths))`.
   *
   * @param paths Simulated price paths
   * @return Estimated option value
   */
  def price(paths: Array[PricePath]): Double = {
    logger.info(s"Pricing with ${paths.length} paths using Longstaff-Schwartz")
    logger.info(s"Call dates: ${option.callDates.mkString(", ")}, Maturity: ${option.maturity}")
    val strategy = calibrate(paths)
    priceWithStrategy(paths, strategy)
  }

  /**
   * Calibrate the exercise strategy by backward induction over the call dates.
   *
   * This performs the full Longstaff-Schwartz regression pass, recording the
   * continuation-value coefficients for every call date that has enough in-the-money
   * paths. The rolled-forward cash flows are used only to build the regression targets;
   * the returned [[ExerciseStrategy]] is what callers reuse (or deliberately do NOT reuse)
   * across finite-difference bumps.
   *
   * @param paths Simulated price paths to calibrate on
   * @return The calibrated exercise strategy (regression coefficients per call date)
   */
  def calibrate(paths: Array[PricePath]): ExerciseStrategy = {
    val numPaths = paths.length

    // Initialize cash flows with terminal payoff
    val cashFlows = Array.fill(numPaths)(0.0)
    val exerciseTimes = Array.fill(numPaths)(option.maturity)

    // Set terminal cash flows (down-and-in barrier payoff)
    for (i <- 0 until numPaths) {
      cashFlows(i) = payoffCalculator.payoff(paths(i), option.maturity)
    }

    logger.debug(s"Average terminal payoff: ${cashFlows.sum / numPaths}")

    val coefficientsByDate = scala.collection.mutable.Map[Int, Array[Double]]()

    // Backward induction through call dates
    for (callDate <- option.callDates.reverse if callDate < option.maturity) {
      logger.debug(s"\nProcessing call date: $callDate")

      // Separate in-the-money paths
      val itmPaths = scala.collection.mutable.ArrayBuffer[(Int, Double, Double)]()

      for (i <- 0 until numPaths) {
        val prices = paths(i).pricesAt(callDate)
        val intrinsic = payoffCalculator.intrinsicValue(prices)

        if (intrinsic > 0) {
          val discountedCashFlow = cashFlows(i) *
            option.discountFactors(exerciseTimes(i)) /
            option.discountFactors(callDate)
          itmPaths += ((i, intrinsic, discountedCashFlow))
        }
      }

      logger.debug(s"In-the-money paths: ${itmPaths.size} / $numPaths")

      if (itmPaths.size >= basisFunctions.dimension + 1) {
        // Perform regression to estimate continuation value
        val (x, y) = buildRegressionData(paths, itmPaths.toArray, callDate)
        val coefficients = leastSquaresRegression(x, y)
        coefficientsByDate(callDate) = coefficients

        logger.debug(s"Regression coefficients: ${coefficients.take(5).mkString(", ")}...")

        // Decide exercise vs continue for each ITM path (updates the roll-forward cash flows)
        var earlyExerciseCount = 0
        for ((pathIdx, intrinsic, _) <- itmPaths) {
          val barrierBreached = paths(pathIdx).barrierBreached(option.barrier, callDate)
          val basis = basisFunctions(paths(pathIdx).pricesAt(callDate), barrierBreached)
          val continuationValue = (basis zip coefficients).map { case (b, c) => b * c }.sum

          if (intrinsic > continuationValue) {
            // Exercise now
            cashFlows(pathIdx) = intrinsic
            exerciseTimes(pathIdx) = callDate
            earlyExerciseCount += 1
          }
        }

        logger.debug(s"Early exercise: $earlyExerciseCount paths")
      }
    }

    ExerciseStrategy(coefficientsByDate.toMap)
  }

  /**
   * Price the option on `paths` using an already-calibrated exercise strategy.
   *
   * No regression is performed here: the stored coefficients define the exercise boundary
   * and are simply applied to decide exercise vs. continue on each path. This is the method
   * the finite-difference engine calls for every bumped scenario when the strategy is frozen.
   *
   * @param paths    Simulated price paths to value
   * @param strategy A strategy previously produced by [[calibrate]]
   * @return Estimated option value
   */
  def priceWithStrategy(paths: Array[PricePath], strategy: ExerciseStrategy): Double = {
    val numPaths = paths.length

    val cashFlows = Array.fill(numPaths)(0.0)
    val exerciseTimes = Array.fill(numPaths)(option.maturity)

    for (i <- 0 until numPaths) {
      cashFlows(i) = payoffCalculator.payoff(paths(i), option.maturity)
    }

    // Backward induction, applying the frozen coefficients (no regression)
    for (callDate <- option.callDates.reverse if callDate < option.maturity) {
      strategy.coefficients.get(callDate) match {
        case Some(coefficients) =>
          for (i <- 0 until numPaths) {
            val prices = paths(i).pricesAt(callDate)
            val intrinsic = payoffCalculator.intrinsicValue(prices)

            if (intrinsic > 0) {
              val barrierBreached = paths(i).barrierBreached(option.barrier, callDate)
              val basis = basisFunctions(prices, barrierBreached)
              val continuationValue = (basis zip coefficients).map { case (b, c) => b * c }.sum

              if (intrinsic > continuationValue) {
                cashFlows(i) = intrinsic
                exerciseTimes(i) = callDate
              }
            }
          }
        case None =>
          // Too few ITM paths at calibration time: no early-exercise decision at this date.
      }
    }

    // Discount all cash flows to present value
    val presentValues = (0 until numPaths).map { i =>
      cashFlows(i) * option.discountFactors(exerciseTimes(i))
    }

    val optionValue = presentValues.sum / numPaths
    val stdError = math.sqrt(presentValues.map(pv => math.pow(pv - optionValue, 2)).sum / numPaths) / math.sqrt(numPaths)

    logger.debug(f"Option value: $optionValue%.4f ± $stdError%.4f")

    optionValue
  }

  /**
   * Build regression data (X matrix and Y vector).
   */
  private def buildRegressionData(
    paths: Array[PricePath],
    itmData: Array[(Int, Double, Double)],
    callDate: Int
  ): (Array[Array[Double]], Array[Double]) = {

    val X = itmData.map { case (pathIdx, _, _) =>
      val barrierBreached = paths(pathIdx).barrierBreached(option.barrier, callDate)
      basisFunctions(paths(pathIdx).pricesAt(callDate), barrierBreached)
    }

    val Y = itmData.map { case (_, _, discountedCashFlow) =>
      discountedCashFlow
    }

    (X, Y)
  }

  /**
   * Ordinary least squares regression.
   * Solves (X'X)β = X'Y for β.
   */
  private def leastSquaresRegression(X: Array[Array[Double]], Y: Array[Double]): Array[Double] = {
    val n = X.length
    val p = X(0).length

    // Compute X'X
    val XtX = Array.ofDim[Double](p, p)
    for (i <- 0 until p; j <- 0 until p) {
      XtX(i)(j) = (0 until n).map(k => X(k)(i) * X(k)(j)).sum
    }

    // Compute X'Y
    val XtY = Array.ofDim[Double](p)
    for (i <- 0 until p) {
      XtY(i) = (0 until n).map(k => X(k)(i) * Y(k)).sum
    }

    // Solve using simple Gaussian elimination (for small systems)
    solveLinearSystem(XtX, XtY)
  }

  /**
   * Solve linear system Ax = b using Gaussian elimination.
   */
  private def solveLinearSystem(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val n = b.length
    val augmented = A.map(_.clone())
    val rhs = b.clone()

    // Forward elimination
    for (i <- 0 until n) {
      // Find pivot
      val maxRow = (i until n).maxBy(row => math.abs(augmented(row)(i)))
      if (maxRow != i) {
        val tempRow = augmented(i)
        augmented(i) = augmented(maxRow)
        augmented(maxRow) = tempRow

        val tempVal = rhs(i)
        rhs(i) = rhs(maxRow)
        rhs(maxRow) = tempVal
      }

      // Eliminate column
      for (j <- i + 1 until n) {
        val factor = augmented(j)(i) / augmented(i)(i)
        for (k <- i until n) {
          augmented(j)(k) -= factor * augmented(i)(k)
        }
        rhs(j) -= factor * rhs(i)
      }
    }

    // Back substitution
    val x = Array.ofDim[Double](n)
    for (i <- (n - 1) to 0 by -1) {
      x(i) = rhs(i)
      for (j <- i + 1 until n) {
        x(i) -= augmented(i)(j) * x(j)
      }
      x(i) /= augmented(i)(i)
    }

    x
  }
}

/**
 * Monte Carlo path generator for multiple correlated assets.
 *
 * @param usePerformance If true, generated paths will calculate performances for barrier monitoring and payoffs
 */
class PathGenerator(
  numAssets: Int,
  initialPrices: Array[Double],
  drifts: Array[Double],
  volatilities: Array[Double],
  correlationMatrix: Array[Array[Double]],
  timeSteps: Array[Double],
  seed: Long = 42L,
  usePerformance: Boolean = false
) extends LazyLogging {

  private val random = new Random(seed)

  require(initialPrices.length == numAssets)
  require(drifts.length == numAssets)
  require(volatilities.length == numAssets)
  require(correlationMatrix.length == numAssets)
  require(correlationMatrix.forall(_.length == numAssets))

  // Compute Cholesky decomposition of correlation matrix
  private val choleskyMatrix = choleskyDecomposition(correlationMatrix)

  /**
   * Generate a single price path for all assets.
   */
  def generatePath(): PricePath = {
    val numSteps = timeSteps.length
    val path = Array.ofDim[Double](numSteps, numAssets)

    // Set initial prices
    path(0) = initialPrices.clone()

    // Generate correlated random walks
    for (t <- 1 until numSteps) {
      val dt = timeSteps(t) - timeSteps(t - 1)
      val sqrtDt = math.sqrt(dt)

      // Generate independent standard normals
      val Z = Array.fill(numAssets)(random.nextGaussian())

      // Apply Cholesky to get correlated normals
      val correlatedZ = Array.ofDim[Double](numAssets)
      for (i <- 0 until numAssets) {
        correlatedZ(i) = (0 until numAssets).map(j => choleskyMatrix(i)(j) * Z(j)).sum
      }

      // Update prices using geometric Brownian motion
      for (i <- 0 until numAssets) {
        val drift = (drifts(i) - 0.5 * volatilities(i) * volatilities(i)) * dt
        val diffusion = volatilities(i) * sqrtDt * correlatedZ(i)
        path(t)(i) = path(t - 1)(i) * math.exp(drift + diffusion)
      }
    }

    PricePath(path, usePerformance)
  }

  /**
   * Generate multiple paths.
   */
  def generatePaths(numPaths: Int): Array[PricePath] = {
    logger.info(s"Generating $numPaths paths for $numAssets assets")
    Array.fill(numPaths)(generatePath())
  }

  /**
   * Cholesky decomposition: finds L such that LL' = A.
   */
  private def choleskyDecomposition(A: Array[Array[Double]]): Array[Array[Double]] = {
    val n = A.length
    val L = Array.ofDim[Double](n, n)

    for (i <- 0 until n) {
      for (j <- 0 to i) {
        var sum = 0.0
        for (k <- 0 until j) {
          sum += L(i)(k) * L(j)(k)
        }

        if (i == j) {
          L(i)(j) = math.sqrt(A(i)(i) - sum)
        } else {
          L(i)(j) = (A(i)(j) - sum) / L(j)(j)
        }
      }
    }

    L
  }
}

/**
 * Main example demonstrating the complete framework.
 *
 * DEMONSTRATES BOTH:
 * 1. PERFORMANCE-BASED payoff (S/S_0) - MOST COMMON in structured products
 * 2. Absolute price payoff - for comparison
 *
 * Toggle by setting usePerformance = true/false
 */
object LongstaffSchwartzWorstOf extends App with LazyLogging {

  logger.info("=" * 80)
  logger.info("LONGSTAFF-SCHWARTZ FOR WORST-OF AMERICAN OPTION WITH DOWN-AND-IN BARRIER")
  logger.info("Using orthogonal Laguerre polynomials (original LS approach)")
  logger.info("=" * 80)

  // CHOOSE PAYOFF TYPE
  val usePerformance = true  // Set to true for performance-based (S/S_0), false for absolute prices

  // Option parameters
  val numAssets = 3
  val strike = if (usePerformance) 1.0 else 100.0     // 100% for performance, 100 for absolute
  val barrier = if (usePerformance) 0.70 else 70.0   // 70% for performance, 70 for absolute
  val maturity = 12   // 12 months
  val callDates = Array(3, 6, 9, 12)  // Quarterly call dates

  logger.info(s"\nPayoff Type: ${if (usePerformance) "PERFORMANCE-BASED (S/S_0)" else "ABSOLUTE PRICE"}")
  logger.info(s"\nOption Structure:")
  logger.info(s"  - Number of assets: $numAssets")
  logger.info(s"  - Strike: $strike ${if (usePerformance) "(100%)" else ""}")
  logger.info(s"  - Barrier (down-and-in): $barrier ${if (usePerformance) "(70%)" else ""}")
  logger.info(s"  - Call dates: ${callDates.mkString(", ")} months")
  logger.info(s"  - Maturity: $maturity months")
  if (usePerformance) {
    logger.info(s"  - Payoff: max(Strike - Worst_Performance, 0) = max($strike - min(S_i/S_i(0)), 0)")
  } else {
    logger.info(s"  - Payoff: max(Strike - Worst_Price, 0) = max($strike - min(S_i), 0)")
  }

  // Market parameters
  val initialPrices = Array(100.0, 100.0, 100.0)
  val riskFreeRate = 0.05
  val drifts = Array.fill(numAssets)(riskFreeRate)
  val volatilities = Array(0.20, 0.25, 0.30)  // Different vols

  // Correlation matrix (positive correlation between assets)
  val correlationMatrix = Array(
    Array(1.0, 0.5, 0.3),
    Array(0.5, 1.0, 0.4),
    Array(0.3, 0.4, 1.0)
  )

  logger.info(s"\nMarket Parameters:")
  logger.info(s"  - Initial prices: ${initialPrices.mkString(", ")}")
  logger.info(s"  - Risk-free rate: ${riskFreeRate * 100}%")
  logger.info(s"  - Volatilities: ${volatilities.map(v => f"${v * 100}%.1f%%").mkString(", ")}")
  logger.info(s"  - Correlations: ρ₁₂=${correlationMatrix(0)(1)}, ρ₁₃=${correlationMatrix(0)(2)}, ρ₂₃=${correlationMatrix(1)(2)}")

  // Time grid (monthly steps)
  val timeSteps = (0 to maturity).map(_.toDouble / 12.0).toArray
  val discountFactors = timeSteps.map(t => math.exp(-riskFreeRate * t))

  // Create option specification
  val option = WorstOfBarrierOption(
    strike = strike,
    barrier = barrier,
    callDates = callDates,
    maturity = maturity,
    discountFactors = discountFactors
  )

  // Create basis functions using Laguerre polynomials (orthogonal, numerically stable)
  val basisFunctions = new LaguerreBasisFunctions(
    numAssets = numAssets,
    maxDegree = 5,  // Can use higher degrees safely with Laguerre!
    includeBarrierInteractions = true,
    normalizeToStrike = !usePerformance,  // Performances already normalized
    strikeLevel = strike,
    usePerformance = usePerformance
  )

  logger.info(s"\nBasis Functions:")
  logger.info(s"  - Type: Laguerre (orthogonal polynomials - original Longstaff-Schwartz)")
  logger.info(s"  - Dimension: ${basisFunctions.dimension}")
  logger.info(s"  - Max degree: 5")
  logger.info(s"  - Includes barrier interactions: Yes")
  if (usePerformance) {
    logger.info(s"  - Uses performances (S/S_0) - naturally normalized around 1.0")
  } else {
    logger.info(s"  - Normalized to strike: Yes")
  }
  logger.info(s"  - Advantages: No multicollinearity, superior numerical stability")

  // Create payoff calculator
  val payoffCalculator = new DownAndInWorstOfPut(strike, barrier, usePerformance)

  // Generate paths
  val pathGenerator = new PathGenerator(
    numAssets = numAssets,
    initialPrices = initialPrices,
    drifts = drifts,
    volatilities = volatilities,
    correlationMatrix = correlationMatrix,
    timeSteps = timeSteps,
    seed = 42L,
    usePerformance = usePerformance
  )

  logger.info(s"\nMonte Carlo Simulation:")
  val numPaths = 10000
  logger.info(s"  - Number of paths: $numPaths")
  logger.info(s"  - Time steps: ${timeSteps.length}")

  val paths = pathGenerator.generatePaths(numPaths)

  // Analyze paths
  val barrierHitCount = paths.count(_.barrierBreached(barrier, maturity))
  val barrierHitRate = barrierHitCount.toDouble / numPaths
  logger.info(s"  - Barrier hit rate: ${(barrierHitRate * 100).formatted("%.2f")}% ($barrierHitCount paths)")

  // Price the option
  logger.info("\n" + "=" * 80)
  logger.info("PRICING")
  logger.info("=" * 80)

  val pricer = new LongstaffSchwartzPricer(option, basisFunctions, payoffCalculator)
  val optionPrice = pricer.price(paths)

  logger.info("\n" + "=" * 80)
  logger.info("RESULTS")
  logger.info("=" * 80)
  logger.info(f"Worst-of American Down-and-In Put Price: $optionPrice%.4f")
  if (usePerformance) {
    logger.info(f"  (Performance-based: payoff on S/S_0)")
    logger.info(f"  Strike: ${strike * 100}%%, Barrier: ${barrier * 100}%%")
  } else {
    logger.info(f"  (Absolute price-based)")
    logger.info(f"  Strike: $strike, Barrier: $barrier")
  }
  logger.info("=" * 80)

  logger.info("""

KEY IMPLEMENTATION NOTES:

1. PERFORMANCE vs ABSOLUTE PRICE:
   - Set usePerformance = true  for S/S_0 payoffs (MOST COMMON in structured products)
   - Set usePerformance = false for absolute price payoffs
   - Everything adjusts automatically: barrier monitoring, payoffs, basis functions

2. PERFORMANCE-BASED ADVANTAGES:
   - Natural normalization (performances ~1.0, no need to normalize by strike)
   - Barrier/strike in percentage terms (e.g., 0.70 = 70%, 1.0 = 100%)
   - Standard in equity-linked notes and worst-of structures

3. BASIS FUNCTION COMPARISON:
   To use standard polynomials instead of Laguerre (not recommended):

   val basisFunctions = new WorstOfPolynomialBasis(
     numAssets = numAssets,
     maxDegree = 3,  // Max 3 for stability with standard polynomials
     includeBarrierInteractions = true
   )

   Laguerre gives more stable and reliable results with higher degrees (5-7).
""")
}
