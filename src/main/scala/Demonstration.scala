import Functions.*
import Polynomials.*
import Vectors.MathVector
import scala.math.*

object Demonstration extends App {

  /*
    Ideas:
      - Product of two polynomials.
      - Find the Taylor-expansion of the exponential function.
      - Estimate sqrt(2) using Newton's method.
      - Find the local maximum Pi of the sine using gradientAscent.
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
  val fewIterations = 3
  val moreIterations = 5
  val initialGuess = 1
  println(s"We start the algorithm with the initialGuess $initialGuess and run $fewIterations iterations.")
  val firstEstimate = p.toFunction().newtonsMethod(1, fewIterations)
  println(s"This gives the estimate $firstEstimate.")
  println()
  val wolframAlphaSqrt2 = 1.4142135624
  println(s"According to wolframalpha.com, sqrt(2) is $wolframAlphaSqrt2 to 10 decimal digits' precision.")
  println(s"To make our estimate more precise, we run $moreIterations iterations instead,")
  val secondEstimate = p.toFunction().newtonsMethod(1, moreIterations)
  println(s"and we get the estimate $secondEstimate, and this is precise enough!")
  println()
  println("Alternatively, we could used the method newtonsMethodPolynomial from the Polynomial class\n" +
    "directly on the Polynomial object p instead of first converting p to a RealFunction object.\n" +
    "While the method newtonsMethod from the class RealFunction uses approximate derivatives,\n" +
    "the newtonsMethodPolynomial uses exact derivatives so we expect this to give better results.")
  println()
  println(s"With this approach we get the following estimate after $fewIterations iterations")
  val newFirstEstimate = p.newtonsMethodPolynomial(1, 3)
  println(newFirstEstimate)
  println(s"which is better than what we got after $fewIterations before.")
  println(s"After $moreIterations iterations, we get")
  val newSecondEstimate = p.newtonsMethodPolynomial(1, moreIterations)
  println(newSecondEstimate)
  println("which is accurate to a precision of 9 decimal digits.")

}
