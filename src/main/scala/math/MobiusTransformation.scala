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

    def apply (o: Quadric): Quadric =
        // the actions is q -> mqm^*
        // our slow/lazy implementation lifts the quadric to a Mobius transformation, 
        // multiplies there, then lowers back to a quadric
        val q = MobiusTransformation(o.a, o.bc, o.bc.conj, o.d)
        val r = this * q * this.dagger
        Quadric(r.a.realpart, r.b, r.d.realpart)

    def scale (z: Complex): MobiusTransformation =
        MobiusTransformation(this.a*z, this.b*z, this.c*z, this.d*z)

    def * (that: MobiusTransformation): MobiusTransformation =
        MobiusTransformation(
            this.a*that.a+this.b*that.c,
            this.a*that.b+this.b*that.d,
            this.c*that.a+this.d*that.c,
            this.c*that.b+this.d*that.d,
        )
    
    def inv: MobiusTransformation = 
        MobiusTransformation(this.d, -this.c, -this.b, this.a) scale  (1/ this.det)

    def dagger: MobiusTransformation =
        MobiusTransformation(this.a.conj, this.c.conj, this.b.conj, this.d.conj)

    def eigenvalues: List[Complex] =
        roots(1, -this.trace, this.det)
