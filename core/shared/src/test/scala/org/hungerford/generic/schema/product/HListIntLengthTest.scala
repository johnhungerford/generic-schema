package org.hungerford.generic.schema.product

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._

import scala.concurrent.Future

class HListIntLengthTest extends AnyFlatSpecLike with Matchers {

    behavior of "HListIntLength"

    it should "return length = 0 for HNil" in {
        HListIntLength[ HNil ].length shouldBe 0
    }

    it should "return length = 1 for Anything :: HNil" in {
        HListIntLength[ Int :: HNil ].length shouldBe 1
        HListIntLength[ Double :: HNil ].length shouldBe 1
        HListIntLength[ String :: HNil ].length shouldBe 1
        HListIntLength[ List[ Vector[ Map[ Future[ BigInt ], Either[ Throwable, String ] ] ] ] :: HNil ].length shouldBe 1
    }

    it should "return correct length for HLists" in {
        HListIntLength[ Int :: String :: Map[ String, Double ] :: Unit :: Any :: String :: HNil ].length shouldBe 6
    }

}
