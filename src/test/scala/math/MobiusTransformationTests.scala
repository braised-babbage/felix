package felix.tests

import utest._
import felix.math.*

object MobiusTransformationTests extends TestSuite {
  val tests = Tests{
    val scale2 = MobiusTransformation(2, 0, 0, 1)
    val addI = MobiusTransformation(1, Complex.I, 0, 1)
    val reciprocate = MobiusTransformation(0, 1, 1, 0)
    val affineMap = MobiusTransformation(2, Complex.I, 0, 1)

    // todo: better equality method
    def ptwiseEq(m1: MobiusTransformation, m2 :MobiusTransformation): Boolean =
        val pts = List(Complex(1,0), Complex(0, 1), Complex(1,1))
        pts.forall(pt => m1(pt) ~= m2(pt))
    
    test("point by affine map"){
        val z = Complex(3, 4)
        assert( affineMap(z) ~= (z*2 + Complex.I))
    }
    test("composition") {
        assert( ptwiseEq(affineMap, addI*scale2))
    }
    test("composition") {
        assert( MobiusTransformation(1,2,3,4) * MobiusTransformation(2, 0, 0, 2)
            == MobiusTransformation(2,4,6,8))
    }
    test("dagger") {
        assert( MobiusTransformation(1, Complex.I, 3, Complex.I*2).dagger
             == MobiusTransformation(1, 3, -Complex.I, -Complex.I*2))
    }

    test("circle by affine map"){
        val center = Complex(1, 1)
        val circle1 = Circle(center, 1)
        val newCenter = affineMap(center).recover.get        
        val expected = Circle(newCenter, 2)
        assert( 
            affineMap(circle1) match {
                case circle2 @ Circle(_,_) => circle2 ~= expected
                case _ => false
            }
        )
    }

    test("line by affine map"){
        val line = Line(scala.math.Pi/4, 1)
        // this has pts: sqrt(1/2)+sqrt(1/2)i  which map to  sqrt(2)+(sqrt(2)+1)i
        //               sqrt(2)                             sqrt(8) + i
        // so direction is the same, but offset is now 2+sqrt(1/2)
        val expected = Line(scala.math.Pi/4, 2+scala.math.sqrt(0.5))
        assert(
            affineMap(line) match {
                case l2 @ Line(_,_) => l2 ~= expected
                case _ => false
            }
        )
    }
  }
}
