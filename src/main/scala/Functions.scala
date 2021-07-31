def factorial(n: Int): Double =
  if n == 0 then
    1
  else
    n * factorial(n - 1)

enum ApproximationMethods:
  case RIGHT, LEFT, AVERAGE

enum IntegrationMethods:
  case RIGHT_ENDPOINT, LEFT_ENDPOINT, MIDPOINT, RANDOM_POINT

import ApproximationMethods.*
import IntegrationMethods.*

case class RealFunction(fct: Double => Double):

  // Arithmetic
  def scale(a: Double): RealFunction =
    RealFunction(x => a * this.fct(x))

  def minus(): RealFunction =
    this.scale(-1)

  def plus(other: RealFunction): RealFunction =
    RealFunction(x => this.fct(x) + other.fct(x))

  def times(other: RealFunction): RealFunction =
    RealFunction(x => this.fct(x) * other.fct(x))

  // Differentiation
  def d(point: Double, approxMethod: ApproximationMethods = RIGHT, dx: Double): Double =
    /*
      Return the (approximate) derivative of this at point.
      Use given approximation method and infinitesimal size
      for the calculations.
     */

    approxMethod match {
      case RIGHT => (this.fct(point + dx) - this.fct(point)) / dx
      case LEFT => (this.fct(point) - this.fct(point - dx)) / dx
      case AVERAGE => (d(point, RIGHT, dx) + d(point, LEFT, dx)) / 2
    }

  def nth_d(point: Double, order: Int = 1, approxMethod: ApproximationMethods = RIGHT, dx: Double = 1): Double =
    /*
      Return the n'th order (approximate) derivative of this.
      Use given approximation method and infinitesimal size
      for the calculations.
     */

    if order == 0 then
      this.fct(point)
    else if order == 1 then
      d(point, approxMethod, dx)
    else
      nth_d(point, order - 1, approxMethod, dx)

  def taylor(center: Double, order: Int, approxMethod: ApproximationMethods = RIGHT, dx: Double = 0.1): Polynomial =
    /*
      Return a Polynomial object representing the Taylor-approximation
      of this, expanded around given center and to given order.
     */

    Polynomial(
      MathVector(
        (0 to order).toVector.map(n => this.nth_d(center, n, approxMethod, dx) / factorial(n))
      )
    )

  // Methods for finding zeros and extrema
  def newtonsMethod(initialGuess: Double,
                    iterations: Int,
                    approxMethod: ApproximationMethods = RIGHT,
                    dx: Double = 0.1): Double =
    /*
      Search for a zero of this using Newton's method starting at initialGuess.
    */

    if iterations == 0 then initialGuess else newtonsMethod(initialGuess - this.fct(initialGuess) / this.d(initialGuess, approxMethod, dx), iterations - 1, approxMethod, dx)

  def gradientDescent(initialGuess: Double,
                      iterations: Int,
                      alpha: Double = 0.5,
                      approxMethod: ApproximationMethods = RIGHT,
                      dx: Double = 0.1): Double =
    /*
      Search for a minimum of this starting at initialGuess using gradient descent with moderator alpha.
    */
    
    if iterations == 0 then 
      initialGuess 
    else 
      gradientDescent(
        initialGuess - alpha * this.d(initialGuess, approxMethod, dx), 
        iterations - 1, 
        alpha, 
        approxMethod, 
        dx
      )

  def gradientAscent(initialGuess: Double,
                     iterations: Int,
                     alpha: Double = 0.5,
                     approxMethod: ApproximationMethods = RIGHT,
                     dx: Double = 0.1): Double =
    /*
      Search for a maximum of this starting at initialGuess using gradient descent with moderator alpha on this.minus.
     */
    this.minus().gradientDescent(initialGuess, iterations, alpha, approxMethod, dx)
    
  def integral(a: Double = 0,
               b: Double = 1,
               fineness: Double = 0.1,
               integrationMethod: IntegrationMethods = LEFT_ENDPOINT): Double =
    val partition: Vector[Double] = RealFunction.partition(a, b, fineness)
    val leftEndpoints: Vector[Double] = partition.dropRight(1)
    val rightEndPoints: Vector[Double] = partition.drop(1)

    val tags: Vector[Double] =
      if integrationMethod == LEFT_ENDPOINT then
        leftEndpoints
      else if integrationMethod == RIGHT_ENDPOINT then
        rightEndPoints
      else if integrationMethod == MIDPOINT then
        leftEndpoints.zip(rightEndPoints).map((left, right) => (left + right) / 2)
      else
        leftEndpoints

    leftEndpoints
      .zip(rightEndPoints)
      .zip(tags)
      .map(t => this.fct(t._2) * (t._1._2 - t._1._1))
      .reduce((a, b) => a + b)


object RealFunction:

  def partition(a: Double, b: Double, dx: Double): Vector[Double] =
    if a > b then
      Vector[Double](b)
    else
      a +: partition(a + dx, b, dx)