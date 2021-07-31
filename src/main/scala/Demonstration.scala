import scala.math._

@main
def main() =

  /*
    We will
      - compute the product of two polynomials.
      - find the Taylor-expansion of the exponential function.
      - estimate sqrt(2) using Newton's method.
      - find the local maximum Pi of the sine using gradientAscent.
   */

  println("Welcome to the Math Show!\n")

  println("Exercise 1: Compute the product of the polynomials 1 - x and 1 + x + x^2.\n")
  println("Solution:")
  val p1 = Polynomial(MathVector(Vector(1, -1)))
  val p2 = Polynomial(MathVector(Vector(1, 1, 1)))
  val prod = p1 times p2
  println(s"The product of $p1 and $p2 is $prod.\n")

  println("Exercise 2: Find the 3rd order Taylor expansion of the exponential function around 0.\n")
  println("Solution:")
  val f = RealFunction(exp)
  val thirdOrderTaylorExpansion = f.taylor(center = 0, order = 3)
  println(thirdOrderTaylorExpansion.toString())

  println("\nExercise 3: Estimate sqrt(2) to a precision of five decimal digits using Newton's method.\n")
  println("Solution:")
  val p = Polynomial(MathVector(Vector(-2, 0, 1)))
  println("sqrt(2) is the positive root of the following polynomial:")
  println(p.toString())

  println("\nWe will use the method newtonsMethod from the RealFunction class to find this root.")
  val fewIterations = 3
  val moreIterations = 5
  val initialGuess = 1
  println(s"We start the algorithm with the initialGuess $initialGuess and run $fewIterations iterations.")
  val firstEstimate = p.toFunction().newtonsMethod(1, fewIterations)
  println(s"This gives the estimate $firstEstimate.\n")
  val wolframAlphaSqrt2 = 1.4142135624
  println(s"According to wolframalpha.com, sqrt(2) is $wolframAlphaSqrt2 to 10 decimal digits' precision.")
  println(s"To make our estimate more precise, we run $moreIterations iterations instead,")
  val secondEstimate = p.toFunction().newtonsMethod(1, moreIterations)
  println(s"and we get the estimate $secondEstimate, and this is precise enough!\n")
  println("Alternatively, we could have used the method newtonsMethodPolynomial from the Polynomial class\n" +
    "directly on the Polynomial object p instead of first converting p to a RealFunction object.\n" +
    "While the method newtonsMethod from the class RealFunction uses approximate derivatives,\n" +
    "the newtonsMethodPolynomial uses exact derivatives so we expect this to give better results.\n")
  println(s"With this approach we get the following estimate after $fewIterations iterations")
  val newFirstEstimate = p.newtonsMethodPolynomial(1, 3)
  println(newFirstEstimate)
  println(s"which is better than what we got after $fewIterations before.")
  println(s"After $moreIterations iterations, we get")
  val newSecondEstimate = p.newtonsMethodPolynomial(1, moreIterations)
  println(newSecondEstimate)
  println("which is accurate to a precision of 9 decimal digits.\n")

  println("Exercise 4: Estimate Pi by finding the local maximum of the sine function in the \n" +
    "interval [0, Pi] (it is Pi/2) using gradient ascent.\n")

  println("Solution:")
  val g = RealFunction(x => sin(x))
  val initialValue = 1
  val iterationsOne = 3
  val iterationsTwo = 10
  println(s"We use gradient ascent with initial guess $initialValue and $iterationsOne iterations.")
  println("This gives")
  val estimateOne = g.gradientAscent(initialValue, iterationsOne)
  println(estimateOne)
  println("The maximum is attained at Pi/2. If we multiply the estimate above with 2,\n" +
    "we should get an approximation of Pi.")
  val piEstimateOne = 2 * estimateOne
  println(piEstimateOne)
  println(s"This estimate is poor. We try with $iterationsTwo iterations instead.")
  println("This gives")
  val estimateTwo = g.gradientAscent(initialValue, iterationsTwo)
  val piEstimateTwo = 2 * estimateTwo
  println(piEstimateTwo)
  println("This is not very impressive either.\n")
  println("The value of the infinitesimal dx used in the approximations involved here\n" +
    "is by default 0.1. Let's see what happens if we choose it smaller.")
  println("For different values of dx, we get the following estimates of Pi:")
  val a = 2 * g.gradientAscent(initialValue, iterationsTwo, dx = 0.01)
  val b = 2 * g.gradientAscent(initialValue, iterationsTwo, dx = 0.001)
  val c = 2 * g.gradientAscent(initialValue, iterationsTwo, dx = 0.0001)
  println("dx = 0.01: " + a)
  println("dx = 0.001: " + b)
  println("dx = 0.0001: " + c)
  println("As expected, the estimates get increasingly better!\n")

  println("This is the end of the Math Show. Thanks for watching!")

