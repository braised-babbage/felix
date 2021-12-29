package felix.test

import utest._
import felix.math.*

object GroupTests extends TestSuite {
    val tests = Tests{
        test("two generators"){
            val a = MobiusTransformation(2,0,0,1)
            val b = MobiusTransformation(1,1,0,1)
            val g = Group('a'->a, 'b'->b)
            assert(g.generators.size == 4)
            assert(g('a') == a)
            assert(g('A') == a.inv)
            assert(g('b') == b)
            assert(g('B') == b.inv)

            assert(g("aA") == MobiusTransformation.identity)
            assert(g("ab") == a * b)
        }
    }
}