package felix.math

final case class Circle(val center: Complex, val radius: Double):
    def quadric: Quadric = 
        Quadric(1, this.center, this.center.norm2 - this.radius*this.radius)

final case class Line(val theta: Double, val offset: Double):
    require(offset >= 0)

    def direction: Complex =
        Complex(scala.math.cos(this.theta), scala.math.sin(this.theta))

    // cos(theta)*x + sin(theta)*y = d
    def quadric: Quadric =
        Quadric(0, direction, offset*2)


final case class Quadric(
    val a: Double, val bc: Complex, val d: Double
):
    // cf. https://mathoverflow.net/a/156208
    // represented as hermitian matrix 
    //  [[a, b+ic],
    //   [b-ic, d]]
    // but we also need b^2 + c^2 > ad

    def defect: Double = this.bc.norm2 - this.a*this.d

    require(defect > 0)

    def extract: Circle | Line =
        if (this.a == 0) then Line(this.bc.argument, this.d / (2 * this.bc.norm))
        else Circle(this.bc / this.a, scala.math.sqrt(this.bc.norm2 / a - this.d))

