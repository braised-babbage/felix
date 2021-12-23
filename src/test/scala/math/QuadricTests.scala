package felix.tests

import utest._
import felix.math.*

object QuadricTests extends TestSuite {
  val tests = Tests {
      val circle = Circle(Complex(3,2), 1)
      val line = Line(scala.math.Pi/4, 1)
      test("recover circle") {
        assert(
            circle.matrix.quadric match { 
                case Some(c @ Circle(_,_)) => circle ~= c
                case _ => false
            }
        )
      }
      test("recover line") {
          assert(
              line.matrix.quadric match {
                  case Some(l @ Line(_, _)) => line ~= l
                  case _ => false
              }
          )
      }
  }
}
