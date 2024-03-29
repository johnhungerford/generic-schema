package org.hungerford.generic.schema.coproduct.subtype

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema

class SubtypeRetrieverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SubtypeRetriever"

    import generic.schema.exports.*

    sealed trait SuperT
    final case class SubT1() extends SuperT
    final case class SubT2() extends SuperT
    final case class SubT3( int : Int ) extends SuperT


    it should "retrieve a subtype from a tuple of subtypes using the type name" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        val st1 = SubtypeRetriever.retrieve( "SubT3", sts )
        st1.typeName shouldBe "SubT3"
        st1.schema.shape.fieldDescriptions.size shouldBe 1
    }

    it should "retrieve a subtype from a tuple of subtypes using the index" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        val st1 = SubtypeRetriever.retrieve( 2, sts )
        st1.typeName shouldBe "SubT3"
        st1.schema.shape.fieldDescriptions.size shouldBe 1
    }

    behavior of "SubtypeRetriever"

    it should "retrieve a subtype from a tuple of subtypes using the type" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        val subt3st = sts.tail.tail.head

        val st1 = SubtypeTypeRetriever.retrieve( t[ SubT3 ], sts )
        st1.typeName shouldBe "SubT3"
        st1.schema.shape.fieldDescriptions.size shouldBe 1
    }

}
