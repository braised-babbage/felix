package felix.math

def roots(a: Complex, b: Complex, c: Complex): List[Complex] =
    val disc = b*b - a*c*4
    val a2 = a*2
    if disc.norm2 == 0 then List(-b/a2)
    else {
        val d = disc.sqrt;
        List((-b+d)/a2, (-b-d)/a2)
    }
