import scala.math.pow


case class Polynomial(coefs: MathVector):

  def apply(point: Double): Double =
  /*
    Evaluate this at point.
   */

    this.coefs.coords.zipWithIndex.map((a, i) => a * pow(point, i)).sum
  def plus(other: Polynomial): Polynomial = Polynomial(this.coefs.plus(other.coefs))
  def degree: Int = this.coefs.coords.lastIndexWhere(a => a != 0)
  def times(other: Polynomial): Polynomial =

    val d = this.degree + other.degree

    Polynomial(
      MathVector(
        (0 to d).toVector
          .map(i => this.coefs.embed_project(i + 1)
            .dot(other.coefs.embed_project(i+1).reverse))
      )
    )


  def derivative: Polynomial =

    Polynomial(
      MathVector(
        this.coefs.coords.zipWithIndex.map((a, i) => i * a).tail
      )
    )

  def toFunction(): RealFunction =
    RealFunction(x => this.apply(x))

  override def toString(): String =
    this.coefs.coords
      .zipWithIndex
      .map((a, i) =>
        if i == 0 then
          s"$a"
        else s"${a}x^$i").mkString(" + ")
    // TODO: Force only two digits after decimal separator.

object Polynomial:

  def apply(coords: Vector[Double]): Polynomial =
    Polynomial(MathVector(coords))

  def newtonsMethod(p: Polynomial, initialGuess: Double, iterations: Int): Double =
    if iterations == 0 then
      initialGuess
    else
      newtonsMethod(p, initialGuess - p(initialGuess) / p.derivative(initialGuess), iterations - 1)
    // TODO: handle division by zero, catch exception

  def gradientDescent(p: Polynomial,
                      initialGuess: Double,
                      iterations: Int,
                      alpha: Double = 0.5): Double =
  /*
    Search for a minimum of f starting at initialGuess using gradient descent with moderator alpha.
  */

    if iterations == 0 then
      initialGuess
    else
      gradientDescent(p, initialGuess - alpha * p.derivative(initialGuess), iterations - 1, alpha)