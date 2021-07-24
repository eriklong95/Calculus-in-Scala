import Vectors.MathVector
import Functions.RealFunction
import scala.math.pow

object Polynomials extends App {

  class Polynomial(val coefs: MathVector):

    // Alternative constructor
    // def this(coefVector: Vector[Double]) =
      // this(MathVector(coefVector))

    def plus(other: Polynomial): Polynomial =
      /*
        Return the sum of this and other.
       */

      Polynomial(this.coefs.plus(other.coefs))

    def times(other: Polynomial): Polynomial =
      /*
        Return the product of this and other.
       */

      val d = this.degree * other.degree

      Polynomial(
        MathVector(
          (0 to d).toVector
            .map(i => this.coefs.embed_project(i + 1)
              .dot(other.coefs.embed_project(i+1).reverse))
        )
      )

    def derivative: Polynomial =
      /*
        Return the derivative of this.
       */

      Polynomial(
        MathVector(
          this.coefs.coords.zipWithIndex.map((a, i) => i * a).drop(0)
        )
      )

    def degree: Int =
      /*
        Return the degree of this. By definition, the
        degree of a polynomial is the index of the
        highest-order non-zero coefficient.
       */

      this.coefs.coords.lastIndexWhere(a => a != 0)

    // Polynomial as function
    def eval(point: Double): Double =
      /*
        Evaluate this at point.
       */

      this.coefs.coords
        .zipWithIndex
        .map((a, i) => a * pow(point, i))
        .sum

    def toFunction(): RealFunction =
      RealFunction(x => this.eval(x))

    def oneIteration(input: Double) =
      input - this.eval(input) / this.derivative.eval(input)

    def rootUsingNewton(initialGuess: Double, iterations: Int): Double =
      // TODO: not working
      if iterations == 0 then
        initialGuess
      else
        rootUsingNewton(oneIteration(initialGuess), iterations - 1)

    override def toString(): String =
      this.coefs.coords.zipWithIndex.map((a, i) => s"${a}x^$i").mkString(" + ")
      // TODO: Force only two digits after decimal separator.

  val p = Polynomial(MathVector(Vector(1000, 1000)))
  println(p.eval(0))
  println(p.toFunction().fct(0))
  println(p.toString())
}
