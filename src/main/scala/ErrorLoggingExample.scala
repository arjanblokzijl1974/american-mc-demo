import com.typesafe.scalalogging.LazyLogging

import scala.util.{Try, Success, Failure}
import java.io.FileNotFoundException

/**
 * Example class demonstrating various error logging patterns in Scala.
 */
class ErrorLoggingExample extends LazyLogging {

  /**
   * Example 1: Basic error logging with exception
   */
  def basicErrorLogging(filename: String): Unit = {
    try {
      val file = scala.io.Source.fromFile(filename)
      // Do something with file
      file.close()
    } catch {
      case e: FileNotFoundException =>
        logger.error(s"File not found: $filename", e)
      case e: Exception =>
        logger.error(s"Unexpected error while reading file: $filename", e)
    }
  }

  /**
   * Example 2: Error logging with context information
   */
  def logErrorWithContext(userId: String, action: String): Unit = {
    try {
      // Simulate some operation
      throw new IllegalStateException("Invalid state")
    } catch {
      case e: Exception =>
        logger.error(s"Error processing action '$action' for user '$userId': ${e.getMessage}", e)
    }
  }

  /**
   * Example 3: Different log levels
   */
  def demonstrateLogLevels(): Unit = {
    logger.trace("This is a TRACE message - most detailed")
    logger.debug("This is a DEBUG message - for debugging")
    logger.info("This is an INFO message - general information")
    logger.warn("This is a WARN message - warning about potential issues")
    logger.error("This is an ERROR message - error occurred")
  }

  /**
   * Example 4: Using Try for functional error handling with logging
   */
  def functionalErrorHandling(value: String): Option[Int] = {
    Try(value.toInt) match {
      case Success(result) =>
        logger.info(s"Successfully parsed value: $result")
        Some(result)
      case Failure(e) =>
        logger.error(s"Failed to parse value '$value' as integer", e)
        None
    }
  }

  /**
   * Example 5: Conditional error logging
   */
  def conditionalLogging(data: List[String]): Unit = {
    if (data.isEmpty) {
      logger.warn("Received empty data list")
      return
    }

    data.foreach { item =>
      if (item.trim.isEmpty) {
        logger.warn(s"Found empty item in data list")
      } else {
        logger.debug(s"Processing item: $item")
      }
    }
  }

  /**
   * Example 6: Error logging with recovery
   */
  def errorWithRecovery(input: String): String = {
    try {
      // Simulate risky operation
      if (input.isEmpty) throw new IllegalArgumentException("Input cannot be empty")
      input.toUpperCase
    } catch {
      case e: IllegalArgumentException =>
        logger.error(s"Invalid input provided: ${e.getMessage}", e)
        "DEFAULT_VALUE" // Return default value
      case e: Exception =>
        logger.error("Unexpected error occurred", e)
        throw e // Re-throw unexpected exceptions
    }
  }

  /**
   * Example 7: Logging errors in for-comprehension
   */
  def complexOperationWithLogging(id: Int): Option[String] = {
    val result = for {
      data <- fetchData(id)
      processed <- processData(data)
      validated <- validateData(processed)
    } yield validated

    result match {
      case Some(value) =>
        logger.info(s"Successfully completed operation for id: $id")
        Some(value)
      case None =>
        logger.error(s"Operation failed for id: $id")
        None
    }
  }

  // Helper methods for Example 7
  private def fetchData(id: Int): Option[String] = {
    if (id > 0) Some(s"data-$id") else {
      logger.warn(s"Invalid id provided: $id")
      None
    }
  }

  private def processData(data: String): Option[String] = {
    Try(data.reverse) match {
      case Success(result) => Some(result)
      case Failure(e) =>
        logger.error(s"Failed to process data: $data", e)
        None
    }
  }

  private def validateData(data: String): Option[String] = {
    if (data.nonEmpty) Some(data) else {
      logger.error("Validation failed: data is empty")
      None
    }
  }
}

/**
 * Main application demonstrating error logging
 */
object ErrorLoggingExample extends App with LazyLogging {
  logger.info("Starting ErrorLoggingExample application")

  val example = new ErrorLoggingExample

  // Run examples
  logger.info("=== Example 1: Basic Error Logging ===")
  example.basicErrorLogging("nonexistent.txt")

  logger.info("\n=== Example 2: Error with Context ===")
  example.logErrorWithContext("user123", "updateProfile")

  logger.info("\n=== Example 3: Different Log Levels ===")
  example.demonstrateLogLevels()

  logger.info("\n=== Example 4: Functional Error Handling ===")
  example.functionalErrorHandling("123")
  example.functionalErrorHandling("abc")

  logger.info("\n=== Example 5: Conditional Logging ===")
  example.conditionalLogging(List("item1", "", "item2"))

  logger.info("\n=== Example 6: Error with Recovery ===")
  val recovered = example.errorWithRecovery("")
  logger.info(s"Recovered value: $recovered")

  logger.info("\n=== Example 7: Complex Operation ===")
  example.complexOperationWithLogging(42)
  example.complexOperationWithLogging(-1)

  logger.info("ErrorLoggingExample application finished")
}
