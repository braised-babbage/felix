package felix.tests

import utest._
import felix.math.*

object MobiusTransformationTests extends TestSuite {
  val tests = Tests{
      test("z -> 2z+i"){
        val m = MobiusTransformation(2, Complex.I, 0, 1)
        val z = Complex(3, 4)
        assert( m(z) ~= (z*2 + Complex.I))
      }
  }
}
