def factorial(n: Int): Double =
  if n == 0 then
    1
  else
    n * factorial(n - 1)

enum ApproximationMethods:
  case RIGHT, LEFT, AVERAGE

enum IntegrationMethods:
  case RIGHT_ENDPOINT, LEFT_ENDPOINT, MIDPOINT

import ApproximationMethods.*
import IntegrationMethods.*

case class RealFunction(fct: Double => Double):
  
  def apply(point: Double): Double =
    this.fct(point)

  // Differentiation
  def derivative(point: Double, order: Int, approxMethod: ApproximationMethods = AVERAGE, dx: Double = 0.1): Double =
    /*
      Return the n'th order (approximate) derivative of this.
      Use given approximation method and infinitesimal size
      for the calculations.
     */

    if order == 0 then
      this(point)
    else
      approxMethod match {
        case RIGHT => (derivative(point + dx, order - 1, approxMethod, dx) - derivative(point, order - 1, approxMethod, dx)) / dx
        case LEFT => (derivative(point, order - 1, approxMethod, dx) - derivative(point - dx, order - 1, approxMethod, dx)) / dx
        case AVERAGE => (derivative(point + dx, order - 1, approxMethod, dx) - derivative(point - dx, order - 1, approxMethod, dx)) / (2 * dx)
      }

  def d(point: Double, approxMethod: ApproximationMethods = AVERAGE, dx: Double = 0.1): Double =
    this.derivative(point, 1, approxMethod, dx)

  def taylor(center: Double, order: Int, approxMethod: ApproximationMethods = AVERAGE, dx: Double = 0.1): Polynomial =
    /*
      Return a Polynomial object representing the Taylor-approximation
      of this, expanded around given center and to given order.
     */

    Polynomial((0 to order).toVector.map(n => this.derivative(center, n, approxMethod, dx) / factorial(n)))

  // Integration
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

  def apply(p: Polynomial): RealFunction =
    p.toFunction()

  // Numerical methods
  def newtonsMethod(f: RealFunction,
                    initialGuess: Double,
                    iterations: Int,
                    approxMethod: ApproximationMethods = AVERAGE,
                    dx: Double = 0.1): Double =
  /*
    Search for a zero of f using Newton's method starting at initialGuess.
  */

    if iterations == 0 then
      initialGuess
    else
      newtonsMethod(
        f,
        initialGuess - f(initialGuess) / f.d(initialGuess, approxMethod, dx),
        iterations - 1,
        approxMethod,
        dx)

  def gradientDescent(f: RealFunction,
                      initialGuess: Double,
                      iterations: Int,
                      alpha: Double = 0.5,
                      approxMethod: ApproximationMethods = AVERAGE,
                      dx: Double = 0.1): Double =
  /*
    Search for a minimum of f starting at initialGuess using gradient descent with moderator alpha.
  */

    if iterations == 0 then
      initialGuess
    else
      gradientDescent(
        f,
        initialGuess - alpha * f.d(initialGuess, approxMethod, dx),
        iterations - 1,
        alpha,
        approxMethod,
        dx
      )

  // Other
  def partition(a: Double, b: Double, dx: Double): Vector[Double] =
    if a > b then
      Vector[Double](b)
    else
      a +: partition(a + dx, b, dx)