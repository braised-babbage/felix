package felix.math


sealed trait Quadric:
    def matrix: HermitianMatrix

case class Circle(val center: Complex, val radius: Double) extends Quadric:
    def matrix: HermitianMatrix = 
        HermitianMatrix(center.norm2 - radius*radius, center, 1)

    def ~= (other: Circle): Boolean =
        (center ~= other.center) && (radius ~= other.radius)

case class Line(val theta: Double, val offset: Double) extends Quadric:
    require(offset >= 0)

    def direction: Complex =
        Complex(scala.math.cos(theta), scala.math.sin(theta))

    def matrix: HermitianMatrix =
        // cos(theta)*x + sin(theta)*y = offset
        HermitianMatrix(offset*2, direction, 0)

    def ~= (other: Line): Boolean =
        (theta ~= other.theta) && (offset ~= other.offset)

final case class HermitianMatrix(
    val a: Double, val bic: Complex, val d: Double
):
    // cf. https://mathoverflow.net/a/156208
    // quadric a - 2Re[z*(b-ic)] + d|z|^2 = 0 represented
    // as hermitian matrix
    //  [[a, b+ic],
    //   [b-ic, d]]
    // but we also need b^2 + c^2 > ad for this give a valid quadric

    def defect: Double = bic.norm2 - a*d

    def quadric: Option[Quadric] =
        if defect <= 0 then None
        else Some(
            if (d ~= 0)
            then Line(bic.argument, a / (2 * bic.norm))
            else {
                val center = bic / d
                Circle(center, scala.math.sqrt(center.norm2 - a/d))
            }
        )

