package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.Subtype
import org.hungerford.generic.schema.types.Zipper
import org.hungerford.generic.schema.Schema
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.deriving.Mirror

class CoproductDeriverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    sealed trait SuperT
    final case class SubT1( int : Int ) extends SuperT
    final case class SubT2( str : String ) extends SuperT

    behavior of "CoproductDeriver"

    it should "derive a simple coproduct" in {
        val mir = summon[Mirror.SumOf[SuperT]]

        summon[ mir.MirroredElemTypes =:= (SubT1, SubT2) ]
        summon[ mir.MirroredElemLabels =:= ("SubT1", "SubT2") ]

        val deriver = CoproductDeriver[ SuperT ]
        val sch = deriver.derive
        sch.subtypeDescriptions.size shouldBe 2
        sch.subtypeDescriptions.head.typeName shouldBe "SubT1"
    }

}
