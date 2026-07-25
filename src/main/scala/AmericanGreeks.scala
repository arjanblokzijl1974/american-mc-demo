import com.typesafe.scalalogging.LazyLogging

/**
 * Central finite-difference delta and gamma for the Longstaff-Schwartz American Monte Carlo
 * pricer, plus a small stability harness.
 *
 * MOTIVATION
 * ----------
 * In production the spot greeks of an American / callable worst-of are often unstable: bumping
 * the spot and re-pricing produces noisy delta and especially noisy gamma. Two ingredients
 * control that noise, and this engine lets you toggle both so you can measure their effect:
 *
 *   1. COMMON RANDOM NUMBERS. Every bumped scenario is generated with the SAME seed, so the
 *      base, up- and down-bumps share the same underlying normals. Without this the greek is
 *      dominated by Monte-Carlo noise. The engine enforces it by construction: the caller
 *      supplies a `generatePaths` closure that re-seeds identically on every call.
 *
 *   2. EXERCISE-STRATEGY TREATMENT (`recalibratePerBump`). The regression coefficients ARE the
 *      exercise boundary. Two choices:
 *        - FROZEN (recalibratePerBump = false): calibrate once on the base paths, then reuse that
 *          [[ExerciseStrategy]] for every bump. The exercise boundary no longer moves between
 *          up/down, so regression noise cancels in the difference. This is the textbook-stable
 *          choice, justified by the boundary being (to first order) insensitive to a small spot
 *          bump.
 *        - RECALIBRATE (recalibratePerBump = true): re-run the regression on each bumped set of
 *          paths, so the boundary is re-fitted per scenario. This mirrors the production setup
 *          that was reported to stabilise the greeks in this codebase.
 *
 *      Running both and comparing the seed-to-seed dispersion of delta/gamma is exactly the
 *      stability test [[AmericanGreeksStability]] performs below.
 *
 * The greek is per asset: delta(i) = dV/dS_i, gamma(i) = d^2V/dS_i^2, bumping one asset's spot
 * at a time. Both use a CENTRAL difference:
 *
 *   delta(i) = ( V(S_i + h) - V(S_i - h) ) / (2h)
 *   gamma(i) = ( V(S_i + h) - 2 V(S_i) + V(S_i - h) ) / h^2
 *
 * with h = relativeBump * S_i(0).
 */

/**
 * Result of a finite-difference greeks computation.
 *
 * @param price Base (unbumped) option value, priced with the base exercise strategy.
 * @param delta Per-asset central-difference delta, dV/dS_i.
 * @param gamma Per-asset central-difference gamma, d^2V/dS_i^2.
 */
case class SpotGreeks(price: Double, delta: Array[Double], gamma: Array[Double]) {
  def numAssets: Int = delta.length

  def report: String = {
    val sb = new StringBuilder
    sb.append(f"Price: $price%.4f\n")
    for (i <- 0 until numAssets) {
      sb.append(f"  asset $i:  delta = ${delta(i)}%+.5f   gamma = ${gamma(i)}%+.5f\n")
    }
    sb.toString
  }
}

/**
 * Central finite-difference greeks engine for a single spot-bump dimension per asset.
 *
 * The engine is deliberately agnostic about HOW paths are built: the caller passes a
 * `generatePaths` function mapping a spot vector to a set of simulated [[PricePath]]s. That
 * closure is responsible for (a) using common random numbers (identical seed on every call) and
 * (b) holding any performance fixings fixed while only the spot moves. See
 * [[AmericanGreeksStability]] for a concrete closure.
 *
 * @param pricer             The Longstaff-Schwartz pricer (provides calibrate / priceWithStrategy).
 * @param baseSpot           Unbumped spot vector, one entry per asset.
 * @param generatePaths      Builds paths for a given spot vector, using common random numbers.
 * @param recalibratePerBump If true, re-calibrate the exercise strategy on every bumped scenario;
 *                           if false, freeze the base strategy across all bumps.
 * @param relativeBump       Bump size as a fraction of each asset's base spot (default 1%).
 */
class FiniteDifferenceGreeksEngine(
  pricer: LongstaffSchwartzPricer,
  baseSpot: Array[Double],
  generatePaths: Array[Double] => Array[PricePath],
  recalibratePerBump: Boolean,
  relativeBump: Double = 0.01
) extends LazyLogging {

  require(relativeBump > 0.0, "relativeBump must be positive")

  private def bumpedSpot(spot: Array[Double], asset: Int, h: Double): Array[Double] = {
    val s = spot.clone()
    s(asset) += h
    s
  }

  /** Compute price, delta and gamma for every asset. */
  def compute(): SpotGreeks = {
    // Calibrate the base exercise strategy once, on the base (unbumped) paths.
    val basePaths = generatePaths(baseSpot)
    val baseStrategy = pricer.calibrate(basePaths)

    // Price a given spot scenario. In frozen mode reuse the base strategy; in recalibrate mode
    // re-fit the boundary on the freshly (common-random-number) simulated bumped paths.
    def priceAt(spot: Array[Double]): Double = {
      val paths = generatePaths(spot)
      val strategy = if (recalibratePerBump) pricer.calibrate(paths) else baseStrategy
      pricer.priceWithStrategy(paths, strategy)
    }

    // Base value uses the base strategy on the base paths (in both modes this is the natural V0,
    // and it is what the central gamma differences against).
    val v0 = pricer.priceWithStrategy(basePaths, baseStrategy)

    val n = baseSpot.length
    val delta = Array.ofDim[Double](n)
    val gamma = Array.ofDim[Double](n)

    for (i <- 0 until n) {
      val h = relativeBump * baseSpot(i)
      val vUp = priceAt(bumpedSpot(baseSpot, i, h))
      val vDown = priceAt(bumpedSpot(baseSpot, i, -h))

      delta(i) = (vUp - vDown) / (2.0 * h)
      gamma(i) = (vUp - 2.0 * v0 + vDown) / (h * h)

      logger.debug(f"asset $i: h=$h%.4f V-=$vDown%.5f V0=$v0%.5f V+=$vUp%.5f " +
        f"delta=${delta(i)}%+.5f gamma=${gamma(i)}%+.5f")
    }

    SpotGreeks(v0, delta, gamma)
  }
}

/**
 * Stability harness: runs the finite-difference greeks under both strategy treatments (frozen vs.
 * recalibrate-per-bump) across a range of Monte-Carlo seeds, and reports the seed-to-seed mean and
 * standard deviation of delta and gamma per asset. The standard deviation is the stability metric:
 * a smaller sigma means a more stable greek.
 *
 * This is a performance-based (S/S_0) worst-of down-and-in put, matching the main example, but with
 * the initial FIXING held fixed while the spot is bumped (see `pathsFor`). Bumping the spot away
 * from the fixing is what makes spot delta well defined for a performance payoff.
 *
 * Run: sbt "runMain AmericanGreeksStability"
 */
object AmericanGreeksStability extends App with LazyLogging {

  // ---- Product & market setup (mirrors LongstaffSchwartzWorstOf) ----
  val usePerformance = true
  val numAssets = 3
  val strike = 1.0            // 100%
  val barrier = 0.70          // 70%
  val maturity = 12           // months
  val callDates = Array(3, 6, 9, 12)

  val fixing = Array(100.0, 100.0, 100.0)   // initial fixing S_0, held fixed under bumps
  val baseSpot = fixing.clone()             // value today at the fixing level
  val riskFreeRate = 0.05
  val drifts = Array.fill(numAssets)(riskFreeRate)
  val volatilities = Array(0.20, 0.25, 0.30)
  val correlationMatrix = Array(
    Array(1.0, 0.5, 0.3),
    Array(0.5, 1.0, 0.4),
    Array(0.3, 0.4, 1.0)
  )

  val timeSteps = (0 to maturity).map(_.toDouble / 12.0).toArray
  val discountFactors = timeSteps.map(t => math.exp(-riskFreeRate * t))

  val option = WorstOfBarrierOption(strike, barrier, callDates, maturity, discountFactors)
  val basisFunctions = new LaguerreBasisFunctions(
    numAssets = numAssets,
    maxDegree = 5,
    includeBarrierInteractions = true,
    normalizeToStrike = !usePerformance,
    strikeLevel = strike,
    usePerformance = usePerformance
  )
  val payoffCalculator = new DownAndInWorstOfPut(strike, barrier, usePerformance)
  val pricer = new LongstaffSchwartzPricer(option, basisFunctions, payoffCalculator)

  // ---- Simulation controls ----
  val numPaths = 20000
  val relativeBump = 0.01                 // 1% central bump
  val seeds = (1L to 12L).toArray         // independent MC estimates for the stability test

  /**
   * Build paths from a given spot with COMMON RANDOM NUMBERS (fixed `seed`) and, for the
   * performance payoff, the initial FIXING held fixed. We reuse the stock PathGenerator to draw
   * the paths from the bumped spot, then rewrap each PricePath so performance = S_t / fixing
   * rather than S_t / bumpedSpot (otherwise the bump cancels and delta is identically zero).
   */
  def pathsFor(seed: Long)(spot: Array[Double]): Array[PricePath] = {
    val gen = new PathGenerator(
      numAssets = numAssets,
      initialPrices = spot,
      drifts = drifts,
      volatilities = volatilities,
      correlationMatrix = correlationMatrix,
      timeSteps = timeSteps,
      seed = seed,
      usePerformance = usePerformance
    )
    val raw = gen.generatePaths(numPaths)
    if (usePerformance) raw.map(_.copy(referencePrices = Some(fixing))) else raw
  }

  /** Run the FD engine over all seeds for one strategy treatment; collect per-seed greeks. */
  def sweep(recalibratePerBump: Boolean): Array[SpotGreeks] = {
    seeds.map { seed =>
      val engine = new FiniteDifferenceGreeksEngine(
        pricer = pricer,
        baseSpot = baseSpot,
        generatePaths = pathsFor(seed),
        recalibratePerBump = recalibratePerBump,
        relativeBump = relativeBump
      )
      engine.compute()
    }
  }

  def meanStd(xs: Array[Double]): (Double, Double) = {
    val n = xs.length
    val mean = xs.sum / n
    val variance = xs.map(x => (x - mean) * (x - mean)).sum / n
    (mean, math.sqrt(variance))
  }

  def summarise(label: String, results: Array[SpotGreeks]): Unit = {
    logger.info("-" * 78)
    logger.info(s"MODE: $label   (${seeds.length} seeds, $numPaths paths, ${relativeBump * 100}% bump)")
    val (pm, ps) = meanStd(results.map(_.price))
    logger.info(f"  price:   mean = $pm%.4f   std = $ps%.4f")
    for (i <- 0 until numAssets) {
      val (dm, ds) = meanStd(results.map(_.delta(i)))
      val (gm, gs) = meanStd(results.map(_.gamma(i)))
      logger.info(f"  asset $i: delta mean=$dm%+.5f std=$ds%.5f   |   gamma mean=$gm%+.5f std=$gs%.5f")
    }
  }

  logger.info("=" * 78)
  logger.info("GREEK STABILITY TEST: worst-of American down-and-in put (performance payoff)")
  logger.info("Central finite differences, common random numbers per seed")
  logger.info("=" * 78)

  val frozen = sweep(recalibratePerBump = false)
  val recalibrated = sweep(recalibratePerBump = true)

  summarise("FROZEN strategy (calibrate once, reuse across bumps)", frozen)
  summarise("RECALIBRATE per bump (re-fit boundary each scenario)", recalibrated)

  logger.info("-" * 78)
  logger.info("Interpretation: compare the per-asset gamma 'std' between the two modes.")
  logger.info("Lower std = more stable greek across Monte-Carlo seeds.")
  logger.info("=" * 78)
}
