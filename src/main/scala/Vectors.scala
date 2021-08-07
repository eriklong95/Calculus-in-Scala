class MathVector(val coords: Vector[Double]):

  def get_coords: Vector[Double] =
    this.coords

  def embed_project(dimension: Int): MathVector =
    /*
      If dimension (=: d) is larger than or equal to size of
      this, this is embedded in d-dimensional space by appending
      enough zeros. Otherwise, d is smaller than size of this,
      and in this case, this is projected onto d-dimensional
      space, i.e., the size - d last coordinates of this are
      deleted.
     */

    val diff = dimension - this.coords.size
    if diff >= 0 then
      MathVector(this.coords ++ Vector.fill(dimension - this.coords.size)(0))
    else
      MathVector(this.coords.take(dimension))

  def plus(other: MathVector): MathVector =
    /*
      Compute vector sum of this and other.
      If these have different length, embed
      shorter one in dimension of longer one
      by adding zeros.
     */

    val dimension = List(this.coords.size, other.coords.size).max
    val this_one = this.embed_project(dimension)
    val other_one = other.embed_project(dimension)

    MathVector(
      this_one.coords.
        zip(other_one.coords)
        .map(pair => pair(0) + pair(1))
    )

  def dot(other: MathVector): Double =
  /*
    Compute inner product of this and other.
    If these have different lengths, project
    longer one to dimension of shorter one.
   */

    this.coords
      .zip(other.coords)
      .map(pair => pair(0) * pair(1))
      .sum

  def reverse: MathVector =
    MathVector(this.coords.reverse)
