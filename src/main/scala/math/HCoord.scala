package felix.math

final case class HCoord(val _1: Complex, val _2: Complex):
    def ~= (that: HCoord) =
        (this._1 ~= that._1) && (this._2 ~= that._2)

    override def toString =
        s"(${this._1}, ${this._2})"

object HCoord:
    given Conversion[Complex, HCoord] = HCoord(_, 1)