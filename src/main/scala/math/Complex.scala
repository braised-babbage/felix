package felix.math

val DOUBLE_COMPARISON_THRESHOLD: Double = 1e-14

extension (d: Double) {
    def ~= (e: Double): Boolean = 
        scala.math.abs(d-e) < DOUBLE_COMPARISON_THRESHOLD
}

final case class Complex(val realpart: Double, val imagpart: Double):
    def + (that: Complex): Complex =
        Complex(this.realpart+that.realpart, this.imagpart+that.imagpart)

    def - (that: Complex): Complex =
        Complex(this.realpart-that.realpart, this.imagpart-that.imagpart)

    def unary_- : Complex =
        Complex(-this.realpart, -this.imagpart)

    def * (that: Complex): Complex =
        Complex(
            this.realpart*that.realpart-this.imagpart*that.imagpart,
            this.realpart*that.imagpart+this.imagpart*that.realpart
        )

    def / (that: Complex): Complex =
        (this * that.conj) * (1 / that.norm2)

    def conj: Complex =
        Complex(this.realpart, -this.imagpart)

    def inv: Complex =
        this.conj / this.norm2

    def norm2: Double = 
        this.realpart*this.realpart + this.imagpart*this.imagpart

    def norm: Double =
        scala.math.sqrt(norm2)

    def argument: Double = 
        scala.math.atan2(this.imagpart, this.realpart)

    def sqrt: Complex =
        Complex.polar(scala.math.sqrt(this.norm), this.argument/2)

    def isAlmostZero: Boolean =
        (this.realpart ~= 0) && (this.imagpart ~= 0)

    def ~= (that: Complex): Boolean =
        (this-that).isAlmostZero

    override def toString = 
        if this.imagpart == 0 then this.realpart.toString
        else if this.realpart == 0 then s"${this.imagpart}i"
        else s"${this.realpart}+${this.imagpart}i"


object Complex:
    given Conversion[Double, Complex] = Complex(_, 0)
    given Conversion[Int, Complex] = Complex(_, 0)

    def polar(r: Double, theta: Double): Complex = 
        Complex(r*scala.math.cos(theta), r*scala.math.sin(theta))

    def I: Complex = Complex(0, 1)