package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TypeInequalityTest extends AnyFlatSpecLike with Matchers {

    case class TestCase[ A, B ]()

    given [ A, B ](
        using
        ev : Ineq[ A, B ],
    ) : TestCase[ A, B ] = TestCase[ A, B ]()

    behavior of "TypeInequality"

    it should "prove types are not equal" in {
        assertCompiles( """summon[ TestCase[ Int, Double ] ]""" )
        assertCompiles( """summon[ Ineq[ Int, Double ] ]""")
    }

    it should "fail to prove that equal types are not equal" in {
        assertDoesNotCompile( """summon[ TestCase[ Int, Int ] ]""" )
        assertDoesNotCompile( """summon[ Ineq[ Int, Int ] ]""")
        summon[ Ineq[ "str", "int" ] ]
    }

}
