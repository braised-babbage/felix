package felix.math

final case class MobiusTransformation(
    val a: Complex, val b: Complex,
    val c: Complex, val d: Complex
):
    def trace: Complex = a+d
    def det: Complex = a*d - b*c
    require(!det.isAlmostZero)

    def apply (o: HCoord): HCoord =
        HCoord(a*o._1+b*o._2, c*o._1+d*o._2)

    def apply (q: Quadric): Quadric =
        val q2 = apply(q.matrix).quadric
        (q2: @unchecked) match {
            case Some(q2) => q2
        }

    def apply (h: HermitianMatrix): HermitianMatrix =
        // the actions is q -> mqm^*
        // our slow/lazy implementation lifts the quadric to a Mobius transformation, 
        // multiplies there, then lowers back to a quadric
        val q = MobiusTransformation(h.a, h.bic, h.bic.conj, h.d)
        val r = this * q * dagger
        HermitianMatrix(r.a.realpart, r.b, r.d.realpart)

    def scale (z: Complex): MobiusTransformation =
        MobiusTransformation(a*z, b*z, c*z, d*z)

    def * (that: MobiusTransformation): MobiusTransformation =
        MobiusTransformation(
            a*that.a+b*that.c,
            a*that.b+b*that.d,
            c*that.a+d*that.c,
            c*that.b+d*that.d,
        )
    
    def inv: MobiusTransformation = 
        MobiusTransformation(d, -b, -c, a) scale  (1/ det)

    def dagger: MobiusTransformation =
        MobiusTransformation(a.conj, c.conj, b.conj, d.conj)

    def eigenvalues: List[Complex] =
        roots(1, -trace, det)

    override def toString: String =
        s"[[${a}, ${b}], [${c}, ${d}]]"

object MobiusTransformation:
    def identity: MobiusTransformation = 
        MobiusTransformation(1, 0, 0, 1)