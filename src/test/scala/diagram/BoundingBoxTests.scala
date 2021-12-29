package felix.test

import utest._
import felix.math.{Circle, Complex}
import felix.diagram.*
import felix.diagram.Bounded.given

object BoundingBoxTests extends TestSuite {
    val tests = Tests{
        test("upper/lower"){
            val b = BoundingBox(-1,-2,3,4)
            println(b.lowerLeft)
            assert(b.lowerLeft == (-1,-2))
            assert(b.upperRight == (2,2))
        }
        test("circle"){
            assert(Circle(Complex(-1,-2), 1).bound == BoundingBox(-2,-3, 2, 2))
        }

        test("join"){
            val b1 = BoundingBox(-1,-2,3,4)
            val b2 = BoundingBox(2,3,4,5)
            assert((b1 join b2) == BoundingBox(-1,-2, 7, 10))
        }
    }
}
