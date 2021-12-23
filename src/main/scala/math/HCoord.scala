package felix.math

final case class HCoord(val _1: Complex, val _2: Complex):
    def recover: Option[Complex] =
        if _2.isAlmostZero then None
        else Some(_1 / _2)

    def ~= (that: HCoord): Boolean =
        (_1 * that._2 - _2 * that._1).isAlmostZero

    override def toString =
        s"(${_1}, ${_2})"

object HCoord:
    given Conversion[Complex, HCoord] = HCoord(_, 1)