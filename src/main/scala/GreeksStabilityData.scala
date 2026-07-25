import com.typesafe.scalalogging.LazyLogging

import java.io.PrintWriter

/**
 * Runs the greek-stability experiment across a spot grid and a set of Monte-Carlo seeds, then
 * writes the results to `viz/greeks_data.json` for the Plotly page (`viz/greeks_stability.html`).
 *
 * It sweeps a parallel spot-level bump (all three spots scaled to `level`, the initial fixing held
 * at 100) and computes price / delta / gamma by CENTRAL differences along the grid:
 *
 *   delta(level) = ( V(level+dl) - V(level-dl) ) / (2 dl)
 *   gamma(level) = ( V(level+dl) - 2 V(level) + V(level-dl) ) / dl^2
 *
 * over a full 2x2 of controls:
 *
 *   EXERCISE BOUNDARY
 *     FROZEN       - calibrate once at level 100 (per seed), reuse across the whole grid.
 *     RECALIBRATE  - re-fit the boundary on each grid level's own paths.
 *
 *   RANDOM NUMBERS ACROSS BUMPS
 *     COMMON (CRN)     - every level on the grid draws the identical normals (seed fixed per seed).
 *                        The bump difference isolates the sensitivity, not Monte-Carlo noise.
 *     INDEPENDENT      - every level draws its own independent normals (seed varies per level).
 *                        Each price carries fresh MC noise, so the central difference divides that
 *                        noise by dl (delta) or dl^2 (gamma).
 *
 * Repeating over independent outer seeds gives the seed-to-seed spread; its standard deviation is
 * the stability metric the page plots.
 *
 * Run: sbt "runMain GreeksStabilityData"
 */
object GreeksStabilityData extends App with LazyLogging {

  // ---- Product & market (mirrors LongstaffSchwartzWorstOf, performance payoff) ----
  val usePerformance = true
  val numAssets = 3
  val strike = 1.0
  val barrier = 0.70
  val maturity = 12
  val callDates = Array(3, 6, 9, 12)

  val fixing = Array(100.0, 100.0, 100.0)
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
    numAssets = numAssets, maxDegree = 5, includeBarrierInteractions = true,
    normalizeToStrike = !usePerformance, strikeLevel = strike, usePerformance = usePerformance
  )
  val payoffCalculator = new DownAndInWorstOfPut(strike, barrier, usePerformance)
  val pricer = new LongstaffSchwartzPricer(option, basisFunctions, payoffCalculator)

  // ---- Sweep controls ----
  val numPaths = 8000
  val grid = (0 to 30).map(i => 70.0 + i * 2.0).toArray  // 70..130 step 2 (dl = 2.0)
  val dl = grid(1) - grid(0)
  val seeds = (1L to 8L).toArray

  /** Paths from a uniform spot `level`, seeded by `seed`, with the initial fixing held fixed. */
  def paths(level: Double, seed: Long): Array[PricePath] = {
    val spot = Array.fill(numAssets)(level)
    val gen = new PathGenerator(
      numAssets, spot, drifts, volatilities, correlationMatrix, timeSteps,
      seed = seed, usePerformance = usePerformance
    )
    gen.generatePaths(numPaths).map(_.copy(referencePrices = Some(fixing)))
  }

  /**
   * Price along the whole grid for one outer seed under one boundary treatment and one RNG regime.
   *
   * @param crn if true every grid level shares the same normals (`seedBase`); if false each level
   *            gets its own independent seed so the finite difference sees fresh MC noise.
   */
  def priceCurve(seedBase: Long, recalibrate: Boolean, crn: Boolean): Array[Double] = {
    val baseStrategy = pricer.calibrate(paths(100.0, seedBase))
    grid.zipWithIndex.map { case (level, k) =>
      val seed = if (crn) seedBase else seedBase * 1000L + k
      val p = paths(level, seed)
      val strategy = if (recalibrate) pricer.calibrate(p) else baseStrategy
      pricer.priceWithStrategy(p, strategy)
    }
  }

  /** Central delta & gamma on the uniform grid; interior points only. */
  def central(prices: Array[Double]): (Array[Double], Array[Double]) = {
    val n = prices.length
    val delta = (1 until n - 1).map(k => (prices(k + 1) - prices(k - 1)) / (2 * dl)).toArray
    val gamma = (1 until n - 1).map(k => (prices(k + 1) - 2 * prices(k) + prices(k - 1)) / (dl * dl)).toArray
    (delta, gamma)
  }

  case class Sweep(price: Array[Array[Double]], delta: Array[Array[Double]], gamma: Array[Array[Double]])

  def sweep(recalibrate: Boolean, crn: Boolean): Sweep = {
    val price = seeds.map(s => priceCurve(s, recalibrate, crn))
    val greeks = price.map(central)
    Sweep(price, greeks.map(_._1), greeks.map(_._2))
  }

  def meanOverSeeds(rows: Array[Array[Double]]): Array[Double] = {
    val n = rows.length
    (0 until rows(0).length).map(j => rows.map(_(j)).sum / n).toArray
  }

  def stdAt(rows: Array[Array[Double]], j: Int): Double = {
    val col = rows.map(_(j))
    val m = col.sum / col.length
    math.sqrt(col.map(x => (x - m) * (x - m)).sum / col.length)
  }

  def meanAt(rows: Array[Array[Double]], j: Int): Double = {
    val col = rows.map(_(j))
    col.sum / col.length
  }

  logger.info(s"Sweeping ${grid.length} levels x ${seeds.length} seeds x $numPaths paths, 2x2 controls...")
  val crnFrozen = sweep(recalibrate = false, crn = true)
  val crnRecal = sweep(recalibrate = true, crn = true)
  logger.info("  common-random-numbers done")
  val ncFrozen = sweep(recalibrate = false, crn = false)
  val ncRecal = sweep(recalibrate = true, crn = false)
  logger.info("  independent-per-bump done")

  val interior = grid.slice(1, grid.length - 1)
  val atm = interior.indices.minBy(i => math.abs(interior(i) - 100.0))

  // ---- Minimal JSON serialisation (no external dependency) ----
  def arr(xs: Array[Double]): String = xs.map(x => f"$x%.8g").mkString("[", ",", "]")
  def mat(xss: Array[Array[Double]]): String = xss.map(arr).mkString("[", ",", "]")

  def curveJson(rows: Array[Array[Double]], x: Array[Double]): String =
    s"""{"seeds":${mat(rows)},"mean":${arr(meanOverSeeds(rows))},"x":${arr(x)}}"""

  def atmJson(rows: Array[Array[Double]]): String =
    f"""{"mean":${meanAt(rows, atm)}%.8g,"std":${stdAt(rows, atm)}%.8g}"""

  def modeJson(s: Sweep): String =
    s"""{"price":${curveJson(s.price, grid)},"delta":${curveJson(s.delta, interior)},""" +
      s""""gamma":${curveJson(s.gamma, interior)},"gamma_atm":${atmJson(s.gamma)},""" +
      s""""delta_atm":${atmJson(s.delta)}}"""

  def groupJson(frozen: Sweep, recal: Sweep): String =
    s"""{"frozen":${modeJson(frozen)},"recal":${modeJson(recal)}}"""

  val json =
    s"""{"grid_full":${arr(grid)},"grid_interior":${arr(interior)},"atm_index":$atm,""" +
      s""""num_paths":$numPaths,"num_seeds":${seeds.length},"bump":$dl,""" +
      s""""crn":${groupJson(crnFrozen, crnRecal)},"nocrn":${groupJson(ncFrozen, ncRecal)}}"""

  val out = new java.io.File("viz/greeks_data.json")
  out.getParentFile.mkdirs()
  val writer = new PrintWriter(out)
  try writer.write(json) finally writer.close()

  logger.info(s"Wrote ${out.getPath}")
  def line(label: String, s: Sweep): Unit =
    logger.info(f"  $label%-26s gamma@100: mean=${meanAt(s.gamma, atm)}%+.5f std=${stdAt(s.gamma, atm)}%.5f  " +
      f"delta@100: mean=${meanAt(s.delta, atm)}%+.5f std=${stdAt(s.delta, atm)}%.5f")
  line("CRN / frozen", crnFrozen)
  line("CRN / recalibrate", crnRecal)
  line("independent / frozen", ncFrozen)
  line("independent / recalibrate", ncRecal)
}
