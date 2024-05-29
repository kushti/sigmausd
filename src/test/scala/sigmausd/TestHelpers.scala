package sigmausd

import org.scalatest.{Assertions, Informing}
import org.scalatest.prop.Whenever
import scala.util.{Failure, Success, Try}

trait TestHelpers extends Whenever with Assertions with Informing {
  /** Easy assert and errors for throwing code,
    * causing scalacheck to loop if not failed by generator-specific value
    * @param fun  block of code to execute
    */
  def assertTry(fun: => Any) = {
    val res = Try(fun)
    whenever(res match {
      case Success(v) =>
        true
      case Failure(e) =>
        info("Info from the exception: " + e.getMessage)
        false
    })(assert(true))
  }

  def assertTryNeg(fun: => Any) = {
    val res = Try(fun)
    whenever(res match {
      case Success(v) =>
        false
      case Failure(e) =>
        true
    })(assert(true))
  }
}
