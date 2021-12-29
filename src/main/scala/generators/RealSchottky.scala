package felix.generators

import felix.math.*

def RealSchottky(s: Double, t: Double): (Group, Map[Char, Circle]) =
    require(0 < s && s < t && t < 1)
    val a = MobiusTransformation(s+t, -2*s*t, -2, s+t)
    val cA = Circle((s+t)/2, (t-s)/2)
    val b = MobiusTransformation(s+t, 2, 2*s*t, s+t)
    val cB = Circle((-1/s - 1/t)/2, (1/s-1/t)/2)
    (Group('a'-> a, 'b'->b),
     Map(
        'a' -> Circle((-t-s)/2, (t-s)/2),
        'A' -> Circle((s+t)/2, (t-s)/2),
        'b' -> Circle((1/s+1/t)/2, (1/s-1/t)/2),
        'B' -> Circle((-1/s-1/t)/2, (1/s-1/t)/2)
     )
    )