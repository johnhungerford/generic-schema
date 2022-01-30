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
        Remover.remove[ 10 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22)
        Remover.remove[ 18 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22)
        Remover.remove[ 21 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
    }

    behavior of "Replacer"

    it should "replace head if I == 0" in {
        Replacer.replace[ 0 ]((3, "hello"), true) shouldBe (true, "hello")
        Replacer.replace[ 0 ](("hello", 543, true), 8.322D) shouldBe (8.322D, 543, true)
        Replacer.replace[ 0 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), "HUH?") shouldBe ("HUH?",2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
    }

    it should "replace element if I > 0" in {
        Replacer.replace[ 1 ]((3, "hello"), true) shouldBe (3, true)
        Replacer.replace[ 1 ](("hello", 543, true), 0.234F) shouldBe ("hello", 0.234F, true)
        Replacer.replace[ 2 ](("hello", 543, true), BigInt( 500 ) ) shouldBe ("hello", 543, BigInt( 500 ))
        Replacer.replace[ 2 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), -5) shouldBe (1,2,-5,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
        Replacer.replace[ 3 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), "HUH?") shouldBe (1,2,3,"HUH?",5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
        Replacer.replace[ 18 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), "HUH?") shouldBe (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,"HUH?",20,21,22)
        Replacer.replace[ 21 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), "HUH?") shouldBe (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,"HUH?")
    }

    behavior of "Retriever"

    it should "retrieve head if I == 0" in {
        Retriever.retrieve[ 0 ]((3, "hello")) shouldBe 3
        Retriever.retrieve[ 0 ](("hello", 543, true)) shouldBe "hello"
        Retriever.retrieve[ 0 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 1
    }

    it should "remove element if I > 0" in {
        Retriever.retrieve[ 1 ]((3, "hello")) shouldBe "hello"
        Retriever.retrieve[ 1 ](("hello", 543, true)) shouldBe 543
        Retriever.retrieve[ 2 ](("hello", 543, true)) shouldBe true
        Retriever.retrieve[ 2 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 3
        Retriever.retrieve[ 3 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 4
        Retriever.retrieve[ 10 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 11
        Retriever.retrieve[ 18 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 19
        Retriever.retrieve[ 21 ]((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)) shouldBe 22
    }


}
