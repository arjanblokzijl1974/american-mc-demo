import com.typesafe.scalalogging.LazyLogging

/**
 * Comparison of different basis function families for Longstaff-Schwartz algorithm.
 *
 * This file compares FOUR different basis function families:
 * 1. Standard Polynomials (1, x, x², x³) - Simple but unstable
 * 2. Laguerre Polynomials - Orthogonal on [0,∞), best for asset prices
 * 3. Hermite Polynomials - Orthogonal on (-∞,∞), best for log-returns
 * 4. Chebyshev Polynomials - Orthogonal on [-1,1], best for bounded domains
 *
 * To avoid code duplication:
 * - LaguerrePolynomials and LaguerreBasisFunctions are defined in LongstaffSchwartzWorstOf.scala
 * - This file adds HermitePolynomials, ChebyshevPolynomials, and their basis functions
 * - WorstOfPolynomialBasis is also in LongstaffSchwartzWorstOf.scala
 *
 * Run: sbt "runMain BasisFunctionComparison"
 *
 * ORTHOGONAL vs STANDARD POLYNOMIALS:
 *
 * Standard Polynomials (1, x, x², x³):
 * ✓ Simple and intuitive
 * ✗ Multicollinearity issues (high correlation between x² and x³)
 * ✗ Numerical instability for higher degrees
 * ✗ Ill-conditioned regression matrices (X'X)
 * ✗ Max practical degree: 3
 *
 * Orthogonal Polynomials (Laguerre, Hermite, Chebyshev):
 * ✓ No multicollinearity (orthogonal by construction)
 * ✓ Better numerical stability
 * ✓ Well-conditioned regression matrices
 * ✓ Better convergence properties
 * ✓ Can use higher degrees (5-7) safely
 * ✓ Original Longstaff-Schwartz paper used weighted Laguerre polynomials
 * ✗ Slightly more complex to implement
 * ✗ Need to choose appropriate weight function or domain
 */

// NOTE: LaguerrePolynomials is defined in LongstaffSchwartzWorstOf.scala

/**
 * Hermite polynomials (probabilist's version): He_n(x).
 *
 * These are orthogonal with respect to the Gaussian measure e^(-x²/2)dx.
 * Natural choice when underlying follows normal distribution (or log-returns).
 *
 * Recursion formula:
 * He_0(x) = 1
 * He_1(x) = x
 * He_{n+1}(x) = x * He_n(x) - n * He_{n-1}(x)
 */
object HermitePolynomials {

  /**
   * Compute Hermite polynomial of degree n at point x.
   */
  def evaluate(n: Int, x: Double): Double = {
    if (n == 0) return 1.0
    if (n == 1) return x

    var He_prev = 1.0  // He_0
    var He_curr = x    // He_1

    for (k <- 1 until n) {
      val He_next = x * He_curr - k * He_prev
      He_prev = He_curr
      He_curr = He_next
    }

    He_curr
  }

  /**
   * Compute all Hermite polynomials up to degree maxDegree at point x.
   */
  def evaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val result = Array.ofDim[Double](maxDegree + 1)
    result(0) = 1.0
    if (maxDegree >= 1) result(1) = x

    for (n <- 1 until maxDegree) {
      result(n + 1) = x * result(n) - n * result(n - 1)
    }

    result
  }

  /**
   * Weighted Hermite basis: He_n(x) * e^(-x²/4).
   * The Gaussian weight improves numerical properties.
   */
  def weightedEvaluate(n: Int, x: Double): Double = {
    evaluate(n, x) * math.exp(-x * x / 4.0)
  }

  def weightedEvaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val weight = math.exp(-x * x / 4.0)
    evaluateAll(maxDegree, x).map(_ * weight)
  }
}

/**
 * Chebyshev polynomials of the first kind: T_n(x).
 *
 * These are orthogonal on [-1, 1] with respect to the weight 1/sqrt(1-x²).
 * Particularly suitable when the domain is bounded.
 *
 * Properties:
 * - Minimize interpolation error (minimax approximation)
 * - Extremely well-conditioned
 * - Natural for bounded domains
 *
 * Recursion formula:
 * T_0(x) = 1
 * T_1(x) = x
 * T_{n+1}(x) = 2x * T_n(x) - T_{n-1}(x)
 */
object ChebyshevPolynomials {

  /**
   * Compute Chebyshev polynomial of degree n at point x.
   * Note: x should be in [-1, 1] for standard Chebyshev properties.
   */
  def evaluate(n: Int, x: Double): Double = {
    if (n == 0) return 1.0
    if (n == 1) return x

    var T_prev = 1.0  // T_0
    var T_curr = x    // T_1

    for (k <- 1 until n) {
      val T_next = 2.0 * x * T_curr - T_prev
      T_prev = T_curr
      T_curr = T_next
    }

    T_curr
  }

  /**
   * Compute all Chebyshev polynomials up to degree maxDegree at point x.
   */
  def evaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val result = Array.ofDim[Double](maxDegree + 1)
    result(0) = 1.0
    if (maxDegree >= 1) result(1) = x

    for (n <- 1 until maxDegree) {
      result(n + 1) = 2.0 * x * result(n) - result(n - 1)
    }

    result
  }

  /**
   * Weighted Chebyshev basis with weight function.
   * For numerical stability, we use a mild weight: sqrt(1 - x²) when |x| < 1.
   */
  def weightedEvaluate(n: Int, x: Double): Double = {
    val weight = if (math.abs(x) < 0.999) math.sqrt(1.0 - x * x) else 0.01
    evaluate(n, x) * weight
  }

  def weightedEvaluateAll(maxDegree: Int, x: Double): Array[Double] = {
    val weight = if (math.abs(x) < 0.999) math.sqrt(1.0 - x * x) else 0.01
    evaluateAll(maxDegree, x).map(_ * weight)
  }

  /**
   * Map asset price to [-1, 1] interval using barriers as bounds.
   * This is crucial for Chebyshev polynomials.
   *
   * @param price Current asset price
   * @param lower Lower bound (e.g., barrier level)
   * @param upper Upper bound (e.g., initial price * some factor)
   */
  def mapToInterval(price: Double, lower: Double, upper: Double): Double = {
    // Map [lower, upper] to [-1, 1]
    2.0 * (price - lower) / (upper - lower) - 1.0
  }
}

// NOTE: LaguerreBasisFunctions is defined in LongstaffSchwartzWorstOf.scala
// (removed duplicate to avoid code duplication)

/**
 * Hermite basis functions for worst-of options with barriers.
 * Uses standardized log-returns as input to Hermite polynomials.
 */
class HermiteBasisFunctions(
  numAssets: Int,
  maxDegree: Int = 5,
  includeBarrierInteractions: Boolean = true,
  referencePrice: Double = 100.0  // Reference for computing log-returns
) extends BasisFunctions with LazyLogging {

  /**
   * Generate basis using weighted Hermite polynomials.
   *
   * Uses log-returns: log(S/S_0) which are approximately normal.
   */
  def apply(prices: AssetPrices, barrierBreached: Boolean): Array[Double] = {
    require(prices.numAssets == numAssets, s"Expected $numAssets assets, got ${prices.numAssets}")

    val basis = scala.collection.mutable.ArrayBuffer[Double]()
    val worst = prices.worst
    val barrierIndicator = if (barrierBreached) 1.0 else 0.0

    // Use log-return as input to Hermite polynomials (natural for GBM)
    val logReturnWorst = math.log(worst / referencePrice)

    // 1. Weighted Hermite polynomials of worst-performing asset log-return
    val hermiteWorst = HermitePolynomials.weightedEvaluateAll(maxDegree, logReturnWorst)
    basis ++= hermiteWorst

    // 2. Hermite polynomials of individual asset log-returns
    for (i <- 0 until numAssets) {
      val logReturn = math.log(prices(i) / referencePrice)
      val hermite = HermitePolynomials.weightedEvaluateAll(math.min(2, maxDegree), logReturn)
      basis ++= hermite
    }

    // 3. Cross terms
    for (i <- 0 until numAssets) {
      val logReturn = math.log(prices(i) / referencePrice)
      basis += HermitePolynomials.weightedEvaluate(1, logReturnWorst) *
               HermitePolynomials.weightedEvaluate(1, logReturn)
    }

    // 4. Barrier interactions
    if (includeBarrierInteractions) {
      basis += barrierIndicator

      for (deg <- 0 to math.min(2, maxDegree)) {
        basis += barrierIndicator * hermiteWorst(deg)
      }
    }

    basis.toArray
  }

  def dimension: Int = {
    var dim = maxDegree + 1
    dim += numAssets * (math.min(2, maxDegree) + 1)
    dim += numAssets

    if (includeBarrierInteractions) {
      dim += 1
      dim += math.min(2, maxDegree) + 1
    }

    dim
  }
}

/**
 * Chebyshev basis functions for worst-of options with barriers.
 * Uses Chebyshev polynomials on bounded domains.
 */
class ChebyshevBasisFunctions(
  numAssets: Int,
  maxDegree: Int = 5,
  includeBarrierInteractions: Boolean = true,
  lowerBound: Double = 50.0,   // Lower price bound (e.g., barrier * 0.7)
  upperBound: Double = 150.0   // Upper price bound (e.g., initial * 1.5)
) extends BasisFunctions with LazyLogging {

  /**
   * Generate basis using weighted Chebyshev polynomials.
   *
   * Key insight: Map prices to [-1, 1] interval where Chebyshev polynomials
   * have optimal approximation properties (minimax property).
   */
  def apply(prices: AssetPrices, barrierBreached: Boolean): Array[Double] = {
    require(prices.numAssets == numAssets, s"Expected $numAssets assets, got ${prices.numAssets}")

    val basis = scala.collection.mutable.ArrayBuffer[Double]()
    val worst = prices.worst
    val barrierIndicator = if (barrierBreached) 1.0 else 0.0

    // Map to [-1, 1] interval
    val mappedWorst = ChebyshevPolynomials.mapToInterval(worst, lowerBound, upperBound)

    // 1. Weighted Chebyshev polynomials of worst-performing asset
    val chebyshevWorst = ChebyshevPolynomials.weightedEvaluateAll(maxDegree, mappedWorst)
    basis ++= chebyshevWorst

    // 2. Chebyshev polynomials of individual assets (lower degree)
    for (i <- 0 until numAssets) {
      val mappedPrice = ChebyshevPolynomials.mapToInterval(prices(i), lowerBound, upperBound)
      val chebyshev = ChebyshevPolynomials.weightedEvaluateAll(math.min(2, maxDegree), mappedPrice)
      basis ++= chebyshev
    }

    // 3. Cross terms: T_1(worst) × T_1(S_i)
    for (i <- 0 until numAssets) {
      val mappedPrice = ChebyshevPolynomials.mapToInterval(prices(i), lowerBound, upperBound)
      basis += ChebyshevPolynomials.weightedEvaluate(1, mappedWorst) *
               ChebyshevPolynomials.weightedEvaluate(1, mappedPrice)
    }

    // 4. Barrier interactions
    if (includeBarrierInteractions) {
      basis += barrierIndicator

      // Barrier × Chebyshev polynomials (low degree)
      for (deg <- 0 to math.min(2, maxDegree)) {
        basis += barrierIndicator * chebyshevWorst(deg)
      }
    }

    basis.toArray
  }

  def dimension: Int = {
    var dim = maxDegree + 1  // Chebyshev of worst
    dim += numAssets * (math.min(2, maxDegree) + 1)  // Chebyshev of individuals
    dim += numAssets  // Cross terms

    if (includeBarrierInteractions) {
      dim += 1  // barrier indicator
      dim += math.min(2, maxDegree) + 1  // barrier × Chebyshev
    }

    dim
  }
}

/**
 * Comparison framework to evaluate different basis function choices.
 */
object BasisFunctionComparison extends App with LazyLogging {

  logger.info("=" * 80)
  logger.info("BASIS FUNCTION COMPARISON FOR LONGSTAFF-SCHWARTZ")
  logger.info("=" * 80)

  // Test data: typical option pricing scenario
  val testPrices = AssetPrices(Array(95.0, 88.0, 102.0))
  val barrierBreached = true
  val numAssets = 3
  val maxDegree = 5
  val strike = 100.0

  logger.info(s"\nTest scenario:")
  logger.info(s"  Asset prices: ${testPrices.prices.mkString(", ")}")
  logger.info(s"  Worst: ${testPrices.worst}")
  logger.info(s"  Barrier breached: $barrierBreached")
  logger.info(s"  Strike: $strike")

  // 1. Standard Polynomial Basis
  logger.info("\n" + "-" * 80)
  logger.info("1. STANDARD POLYNOMIAL BASIS")
  logger.info("-" * 80)

  val polyBasis = new WorstOfPolynomialBasis(numAssets, maxDegree = 3, includeBarrierInteractions = true)
  val polyValues = polyBasis(testPrices, barrierBreached)

  logger.info(s"Dimension: ${polyBasis.dimension}")
  logger.info(s"Basis values (first 10): ${polyValues.take(10).map(v => f"$v%.4f").mkString(", ")}")
  logger.info(s"Value range: [${polyValues.min}, ${polyValues.max}]")
  logger.info(s"Condition number proxy (max/min ratio): ${math.abs(polyValues.max / polyValues.filter(_ != 0).min)}")

  // Check for multicollinearity by looking at correlation-like measures
  logger.info("\nPros: Simple, intuitive")
  logger.info("Cons: Potential multicollinearity, numerical instability for high degrees")

  // 2. Laguerre Polynomial Basis
  logger.info("\n" + "-" * 80)
  logger.info("2. LAGUERRE POLYNOMIAL BASIS (Original Longstaff-Schwartz)")
  logger.info("-" * 80)

  val laguerreBasis = new LaguerreBasisFunctions(
    numAssets,
    maxDegree = maxDegree,
    includeBarrierInteractions = true,
    normalizeToStrike = true,
    strikeLevel = strike
  )
  val laguerreValues = laguerreBasis(testPrices, barrierBreached)

  logger.info(s"Dimension: ${laguerreBasis.dimension}")
  logger.info(s"Basis values (first 10): ${laguerreValues.take(10).map(v => f"$v%.4f").mkString(", ")}")
  logger.info(s"Value range: [${laguerreValues.min}, ${laguerreValues.max}]")

  logger.info("\nPros: Orthogonal (no multicollinearity), numerically stable, proven in practice")
  logger.info("Cons: Slightly more complex, requires normalization")
  logger.info("Best for: Positive-valued variables (asset prices), general purpose")

  // 3. Hermite Polynomial Basis
  logger.info("\n" + "-" * 80)
  logger.info("3. HERMITE POLYNOMIAL BASIS")
  logger.info("-" * 80)

  val hermiteBasis = new HermiteBasisFunctions(
    numAssets,
    maxDegree = maxDegree,
    includeBarrierInteractions = true,
    referencePrice = strike
  )
  val hermiteValues = hermiteBasis(testPrices, barrierBreached)

  logger.info(s"Dimension: ${hermiteBasis.dimension}")
  logger.info(s"Basis values (first 10): ${hermiteValues.take(10).map(v => f"$v%.4f").mkString(", ")}")
  logger.info(s"Value range: [${hermiteValues.min}, ${hermiteValues.max}]")

  logger.info("\nPros: Orthogonal, natural for normal distributions (log-returns)")
  logger.info("Cons: Works on log-returns, less intuitive")
  logger.info("Best for: When underlying returns are approximately normal")

  // 4. Chebyshev Polynomial Basis
  logger.info("\n" + "-" * 80)
  logger.info("4. CHEBYSHEV POLYNOMIAL BASIS")
  logger.info("-" * 80)

  val chebyshevBasis = new ChebyshevBasisFunctions(
    numAssets,
    maxDegree = maxDegree,
    includeBarrierInteractions = true,
    lowerBound = 50.0,   // Below barrier
    upperBound = 150.0   // Above initial
  )
  val chebyshevValues = chebyshevBasis(testPrices, barrierBreached)

  logger.info(s"Dimension: ${chebyshevBasis.dimension}")
  logger.info(s"Basis values (first 10): ${chebyshevValues.take(10).map(v => f"$v%.4f").mkString(", ")}")
  logger.info(s"Value range: [${chebyshevValues.min}, ${chebyshevValues.max}]")

  logger.info("\nPros: Minimax approximation, extremely well-conditioned, natural for bounded domains")
  logger.info("Cons: Requires choosing bounds, less natural for unbounded price processes")
  logger.info("Best for: Options with known price ranges, barrier options with well-defined bounds")

  // Numerical stability comparison
  logger.info("\n" + "=" * 80)
  logger.info("NUMERICAL STABILITY COMPARISON")
  logger.info("=" * 80)

  def computeConditionNumberProxy(values: Array[Double]): Double = {
    val nonZero = values.filter(v => math.abs(v) > 1e-10)
    if (nonZero.isEmpty) Double.PositiveInfinity
    else math.abs(nonZero.max / nonZero.min)
  }

  logger.info(f"\nCondition number proxy (lower is better):")
  logger.info(f"  Standard Polynomial: ${computeConditionNumberProxy(polyValues)}%.2e")
  logger.info(f"  Laguerre:           ${computeConditionNumberProxy(laguerreValues)}%.2e")
  logger.info(f"  Hermite:            ${computeConditionNumberProxy(hermiteValues)}%.2e")
  logger.info(f"  Chebyshev:          ${computeConditionNumberProxy(chebyshevValues)}%.2e")

  // Recommendations
  logger.info("\n" + "=" * 80)
  logger.info("RECOMMENDATIONS")
  logger.info("=" * 80)

  logger.info("""
|Basis Function   | Best For                           | Numerical Stability | Complexity | Domain      |
|-----------------|------------------------------------|---------------------|------------|-------------|
|Standard Poly    | Quick prototyping, low degree      | Poor (high degree)  | Simple     | Unbounded   |
|Laguerre         | General purpose, asset prices      | Excellent           | Medium     | [0, ∞)      |
|Hermite          | Log-normal models, log-returns     | Excellent           | Medium     | (-∞, ∞)     |
|Chebyshev        | Bounded domains, barrier options   | Excellent           | Medium     | [-1, 1]     |

RECOMMENDATION FOR YOUR CASE (Worst-of American with Barrier):

✓ LAGUERRE POLYNOMIALS - Primary recommendation:
  1. Original Longstaff-Schwartz paper used them (proven track record)
  2. Excellent numerical stability (important for multiple regression)
  3. Natural for positive asset prices (domain [0, ∞))
  4. Orthogonality eliminates multicollinearity issues
  5. Better convergence with higher degrees (5-7)
  6. No need to specify bounds

✓ CHEBYSHEV POLYNOMIALS - Strong alternative for barrier options:
  1. Optimal approximation properties (minimax theorem)
  2. Natural bounded domain matches barrier structure
  3. Extremely well-conditioned (condition number close to 1)
  4. Excellent when you know price will stay in a range
  5. Requires choosing upper/lower bounds carefully

Other alternatives:
- Hermite: If working in log-return space
- Standard polynomials: ONLY for low degrees (≤3) and prototyping
""")

  logger.info("\n" + "=" * 80)
  logger.info("USAGE EXAMPLE")
  logger.info("=" * 80)

  logger.info("""
OPTION 1: Laguerre basis (general purpose, proven):

val basisFunctions = new LaguerreBasisFunctions(
  numAssets = 3,
  maxDegree = 5,              // Can use higher degrees safely
  includeBarrierInteractions = true,
  normalizeToStrike = true,
  strikeLevel = 100.0
)

OPTION 2: Chebyshev basis (excellent for barrier options):

val basisFunctions = new ChebyshevBasisFunctions(
  numAssets = 3,
  maxDegree = 5,
  includeBarrierInteractions = true,
  lowerBound = 50.0,          // Below barrier (e.g., barrier * 0.7)
  upperBound = 150.0          // Above initial (e.g., initial * 1.5)
)

Then price as usual:
val pricer = new LongstaffSchwartzPricer(option, basisFunctions, payoffCalculator)
val price = pricer.price(paths)

Both Laguerre and Chebyshev support higher degrees (5-7) safely!
""")

  logger.info("=" * 80)
}
