package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike
import scala.compiletime.ops.int.-

class TupleOpsTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Remover"

    it should "remove head if I == 0" in {
        Remover.remove[ 0 ]((3, "hello")) shouldBe Tuple1("hello")
        Remover.remove[ 0 ](("hello", 543, true)) shouldBe (543, true)
        Remover.remove[ 0 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
    }

    it should "remove element if I > 0" in {
        Remover.remove[ 1 ]((3, "hello")) shouldBe Tuple1(3)
        Remover.remove[ 1 ](("hello", 543, true)) shouldBe ("hello", true)
        Remover.remove[ 2 ](("hello", 543, true)) shouldBe ("hello", 543)
        Remover.remove[ 2 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
        Remover.remove[ 3 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
    }


}
