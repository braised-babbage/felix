package felix.test

import utest._
import felix.math.*

object ComplexTests extends TestSuite {
    val tests = Tests{
        val zero: Complex = 0
        val one: Complex = 1
        val i: Complex = Complex.I

        test("add"){ assert(one + i == Complex(1,1)) }
        test("add"){ assert(one + one == Complex(2, 0)) }
        test("add"){ assert(zero+i == i) }
        test("sub"){ assert(i - i == zero) }
        test("neg"){ assert(-i == Complex(0,-1)) }
        test("mul"){ assert(zero*i == zero) }
        test("mul"){ assert(i*i == Complex(-1,0)) }
        test("mul"){ assert(i*one == i) }
        test("div"){ assert(i / i == one) }
        test("div"){ assert(one / i == -i) }
        test("norm"){ assert(i.norm == 1) }
        test("norm"){ assert(Complex(3,4).norm == 5) }
        test("inv"){ assert(i.inv == Complex(0,-1)) }
        test("inv"){ assert(Complex(1,1).inv * Complex(1,1) == one) }
        test("conj"){ assert(i.conj == -i) }
        test("sqrt"){ assert((-1: Complex).sqrt ~= i) }
        test("arg"){ assert(i.argument ~= scala.math.Pi/2) }
        test("polar"){ assert(Complex.polar(1,scala.math.Pi / 2) ~= i)}
    }
}
