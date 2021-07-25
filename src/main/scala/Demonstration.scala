import Functions.*
import Polynomials.*
import Vectors.MathVector
import scala.math.*

object Demonstration extends App {

  /*
    Ideas:
      - Find the Taylor-expansion of the exponential function.
      - Estimate sqrt(2) using Newton's method.
      - Find minimum of (...) using gradientDescent.
   */

  println("Welcome to the Math Show!")
  println()
  println("Exercise 1: Find the 3rd order Taylor expansion of the exponential function around 0.")
  println()
  println("Solution:")
  val f = RealFunction(exp)
  val thirdOrderTaylorExpansion = f.taylor(center = 0, order = 3)
  println(thirdOrderTaylorExpansion.toString())
  println()
  println("Exercise 2: Estimate sqrt(2) to a precision of five decimal digits using Newton's method.")
  println()
  println("Solution:")
  val p = Polynomial(MathVector(Vector(-2, 0, 1)))
  println("sqrt(2) is the positive root of the following polynomial:")
  println(p.toString())
  println()
  println("We will use Newton's method to find this root.")
  println("We start the algorithm at 1 and run 3 iterations.")
  val firstEstimate = p.toFunction().newtonsMethod(1, 3)
  println(s"This gives the estimate $firstEstimate.")
  println()
  val wolframAlphaSqrt2 = 1.4142135624
  println(s"According to wolframalpha.com, sqrt(2) is $wolframAlphaSqrt2 to 10 decimal digits' precision.")
  println("To make our estimate more precise, we run 5 iterations instead,")
  val secondEstimate = p.toFunction().newtonsMethod(1, 5)
  println(s"and we get the estimate $secondEstimate, and this is precise enough!")
  println()
  println("Alternatively, we could used the method newtonsMethodPolynomial directly on")
  println("the Polynomial object p instead of first converting p to a RealFunction object.")
  println(s"This approach we get the following estimate after ${3} iterations")
  val newFirstEstimate = p.newtonsMethodPolynomial(1, 3)
  println(newFirstEstimate)
  println(s"After ${5} iterations, we get")
  val newSecondEstimate = p.newtonsMethodPolynomial(1, 5)
  println(newSecondEstimate)
  println("Surprisingly enough, the polynomial method seems to converge slower.")
  println(s"After ${10} iterations, we get")
  val newThirdEstimate = p.newtonsMethodPolynomial(1, 10)
  println(newThirdEstimate)
}
