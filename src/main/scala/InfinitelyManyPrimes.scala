import com.typesafe.scalalogging.LazyLogging

/**
 * Demonstrates Euclid's proof that there are infinitely many primes.
 *
 * The proof works as follows:
 * 1. Start with any finite list of primes
 * 2. Multiply them all together and add 1
 * 3. This new number is either prime itself, or has a prime factor
 * 4. Either way, we found a prime not in our original list
 * 5. Therefore, no finite list can contain all primes
 */
object InfinitelyManyPrimes extends App with LazyLogging {

  /**
   * Check if a number is prime.
   */
  def isPrime(n: Long): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else if (n % 2 == 0) false
    else {
      val sqrtN = math.sqrt(n.toDouble).toLong
      !(3L to sqrtN by 2).exists(i => n % i == 0)
    }
  }

  /**
   * Find the smallest prime factor of n.
   */
  def smallestPrimeFactor(n: Long): Long = {
    if (n <= 1) 1L
    else if (n % 2 == 0) 2L
    else {
      val sqrtN = math.sqrt(n.toDouble).toLong
      (3L to sqrtN by 2).find(i => n % i == 0).getOrElse(n)
    }
  }

  /**
   * Generate the first n primes.
   */
  def firstNPrimes(n: Int): List[Long] = {
    def findPrimes(count: Int, current: Long, acc: List[Long]): List[Long] = {
      if (count == 0) acc.reverse
      else if (isPrime(current)) findPrimes(count - 1, current + 1, current :: acc)
      else findPrimes(count, current + 1, acc)
    }
    findPrimes(n, 2L, List.empty)
  }

  /**
   * Euclid's proof: Given a list of primes, find a new prime not in the list.
   */
  def euclidProof(primes: List[Long]): (Long, String) = {
    // Multiply all primes together and add 1
    val product = primes.product
    val euclidNumber = product + 1

    logger.info(s"Given primes: ${primes.mkString(", ")}")
    logger.info(s"Product of all primes: $product")
    logger.info(s"Euclid number (product + 1): $euclidNumber")

    if (isPrime(euclidNumber)) {
      logger.info(s"$euclidNumber is prime itself!")
      (euclidNumber, "prime itself")
    } else {
      val primeFactor = smallestPrimeFactor(euclidNumber)
      logger.info(s"$euclidNumber is not prime, but has prime factor: $primeFactor")

      // Verify this prime factor is not in original list
      if (!primes.contains(primeFactor)) {
        logger.info(s"✓ Found new prime $primeFactor not in original list!")
      }

      (primeFactor, "prime factor")
    }
  }

  /**
   * Demonstrate the proof multiple times with increasing sets of primes.
   */
  def demonstrateInfinity(iterations: Int): Unit = {
    logger.info("=" * 70)
    logger.info("DEMONSTRATING EUCLID'S PROOF OF INFINITELY MANY PRIMES")
    logger.info("=" * 70)
    logger.info("")

    var currentPrimes = List.empty[Long]
    var allFoundPrimes = Set.empty[Long]

    for (i <- 1 to iterations) {
      logger.info(s"\n--- Iteration $i ---")

      if (currentPrimes.isEmpty) {
        // Start with first prime
        currentPrimes = List(2L)
        allFoundPrimes += 2L
        logger.info("Starting with prime: 2")
      } else {
        val (newPrime, howFound) = euclidProof(currentPrimes)

        if (!allFoundPrimes.contains(newPrime)) {
          logger.info(s"→ New prime discovered: $newPrime (as $howFound)")
          currentPrimes = (currentPrimes :+ newPrime).sorted
          allFoundPrimes += newPrime
        }
      }

      logger.info(s"Current set of primes: ${currentPrimes.mkString(", ")}")
      logger.info(s"Total primes found: ${allFoundPrimes.size}")
    }

    logger.info("")
    logger.info("=" * 70)
    logger.info("CONCLUSION")
    logger.info("=" * 70)
    logger.info(s"Started with 0 primes, found ${allFoundPrimes.size} primes through iteration.")
    logger.info("Each iteration proves we can always find a new prime.")
    logger.info("Therefore, there must be infinitely many primes!")
    logger.info("")
    logger.info(s"All discovered primes: ${allFoundPrimes.toList.sorted.mkString(", ")}")
  }

  /**
   * Visual demonstration showing the pattern.
   */
  def visualDemonstration(): Unit = {
    logger.info("\n" + "=" * 70)
    logger.info("VISUAL DEMONSTRATION")
    logger.info("=" * 70)
    logger.info("")

    val examples = List(
      List(2L),
      List(2L, 3L),
      List(2L, 3L, 5L),
      List(2L, 3L, 5L, 7L),
      List(2L, 3L, 5L, 7L, 11L),
      List(2L, 3L, 5L, 7L, 11L, 13L)
    )

    examples.foreach { primes =>
      val product = primes.product
      val euclidNum = product + 1
      val isPrimeItself = isPrime(euclidNum)

      if (isPrimeItself) {
        logger.info(f"${primes.mkString(" × ")} + 1 = $euclidNum%-6d → PRIME!")
      } else {
        val factor = smallestPrimeFactor(euclidNum)
        logger.info(f"${primes.mkString(" × ")} + 1 = $euclidNum%-6d → has prime factor $factor")
      }
    }
  }

  // Run the demonstrations
  logger.info("\n\nDemonstrating that there are INFINITELY MANY PRIMES\n")

  visualDemonstration()
  demonstrateInfinity(10)

  logger.info("\n" + "=" * 70)
  logger.info("QED: There are infinitely many primes!")
  logger.info("=" * 70)
}
