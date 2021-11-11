//package org.hungerford.generic.schema.product
//
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.scalatest.matchers.should.Matchers
//
//import scala.concurrent.Future
//
//class TupleIntLengthTest extends AnyFlatSpecLike with Matchers {
//
//    behavior of "TupleIntLength"
//
//    it should "return length = 0 for EmptyTuple" in {
//        TupleIntLength[ EmptyTuple ].length shouldBe 0
//    }
//
//    it should "return length = 1 for Anything *: EmptyTuple" in {
//        TupleIntLength[ Int *: EmptyTuple ].length shouldBe 1
//        TupleIntLength[ Double *: EmptyTuple ].length shouldBe 1
//        TupleIntLength[ String *: EmptyTuple ].length shouldBe 1
//        TupleIntLength[ List[ Vector[ Map[ Future[ BigInt ], Either[ Throwable, String ] ] ] ] *: EmptyTuple ].length shouldBe 1
//    }
//
//    it should "return correct length for Tuples" in {
//        TupleIntLength[ Int *: String *: Map[ String, Double ] *: Unit *: Any *: String *: EmptyTuple ].length shouldBe 6
//    }
//
//}
