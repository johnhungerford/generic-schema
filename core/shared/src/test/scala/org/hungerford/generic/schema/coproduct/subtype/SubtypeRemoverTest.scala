package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.Schema
import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*

class SubtypeRemoverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SubtypeRemover"

    sealed trait SuperT
    final case class SubT1() extends SuperT
    final case class SubT2() extends SuperT
    final case class SubT3( int : Int ) extends SuperT


    it should "remove a subtype from a tuple of subtypes based on the name" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions
        sts.size shouldBe 3

        val newSts = SubtypeRemover.remove( "SubT3", sts )
        newSts.size shouldBe 2
        newSts.head.typeName shouldBe "SubT1"
        newSts.tail.head.typeName shouldBe "SubT2"

        val newSts2 = SubtypeRemover.remove( "SubT2", sts )
        newSts2.size shouldBe 2
        newSts2.head.typeName shouldBe "SubT1"
        newSts2.tail.head.typeName shouldBe "SubT3"

        val newSts3 = SubtypeRemover.remove( "SubT1", sts )
        newSts3.size shouldBe 2
        newSts3.head.typeName shouldBe "SubT2"
        newSts3.tail.head.typeName shouldBe "SubT3"

        SubtypeRemover.remove("SubT1", SubtypeRemover.remove( "SubT3", SubtypeRemover.remove( "SubT2", sts ) ) ) shouldBe EmptyTuple
    }

}
