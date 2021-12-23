package felix.math

val DOUBLE_COMPARISON_THRESHOLD: Double = 1e-14

extension (d: Double) {
    def ~= (e: Double): Boolean = 
        scala.math.abs(d-e) < DOUBLE_COMPARISON_THRESHOLD
}

final case class Complex(val realpart: Double, val imagpart: Double):
    def + (that: Complex): Complex =
        Complex(realpart+that.realpart, imagpart+that.imagpart)

    def - (that: Complex): Complex =
        Complex(realpart-that.realpart, imagpart-that.imagpart)

    def unary_- : Complex =
        Complex(-realpart, -imagpart)

    def * (that: Complex): Complex =
        Complex(
            realpart*that.realpart-imagpart*that.imagpart,
            realpart*that.imagpart+imagpart*that.realpart
        )

    def / (that: Complex): Complex =
        (this * that.conj) * (1 / that.norm2)

    def conj: Complex =
        Complex(realpart, -imagpart)

    def inv: Complex =
        conj / norm2

    def norm2: Double = 
        realpart*realpart + imagpart*imagpart

    def norm: Double =
        scala.math.sqrt(norm2)

    def argument: Double = 
        scala.math.atan2(imagpart, realpart)

    def sqrt: Complex =
        Complex.polar(scala.math.sqrt(norm), argument/2)

    def isAlmostZero: Boolean =
        (realpart ~= 0) && (imagpart ~= 0)

    def ~= (that: Complex): Boolean =
        (this-that).isAlmostZero

    override def toString = 
        if imagpart == 0 then realpart.toString
        else if realpart == 0 then s"${imagpart}i"
        else s"${realpart}${if imagpart > 0 then '+' else '-'}${imagpart.abs}i"


object Complex:
    given Conversion[Double, Complex] = Complex(_, 0)
    given Conversion[Int, Complex] = Complex(_, 0)

    def polar(r: Double, theta: Double): Complex = 
        Complex(r*scala.math.cos(theta), r*scala.math.sin(theta))

    def I: Complex = Complex(0, 1)