package org.hungerford.generic.schema.coproduct.subtype

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.Schema
import generic.schema.exports.*

class SubtypeReplacerTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SubtypeReplacer"

    behavior of "SubtypeRemover"

    sealed trait SuperT
    final case class SubT1() extends SuperT
    final case class SubT2() extends SuperT
    final case class SubT3( int : Int ) extends SuperT

    it should "replace a subtype using the name" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        case class SubT4( str : String )

        val newSt = Subtype.builder[ Int, SubT4, String, "str" ]
          .typeName( "test-subtype" )
          .toSuper( _.str.length )
          .fromSuper( v => Some( SubT4( v.toString ) ) )
          .discriminatorValue( "test-value" )
          .fromSchema( Schema.derived )
          .build

        val newSts = SubtypeReplacer.replace( "SubT2", sts, newSt )
        newSts.size shouldBe 3
        newSts.head.typeName shouldBe "SubT1"
        newSts.tail.head.typeName shouldBe "test-subtype"
        newSts.tail.head shouldBe newSt
    }

    it should "replace a subtype using the index" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        case class SubT4( str : String )

        val newSt = Subtype.builder[ Int, SubT4, String, "str" ]
          .typeName( "test-subtype" )
          .toSuper( _.str.length )
          .fromSuper( v => Some( SubT4( v.toString ) ) )
          .discriminatorValue( "test-value" )
          .fromSchema( Schema.derived )
          .build

        val newSts = SubtypeReplacer.replace( 1, sts, newSt )
        newSts.size shouldBe 3
        newSts.head.typeName shouldBe "SubT1"
        newSts.tail.head.typeName shouldBe "test-subtype"
        newSts.tail.head shouldBe newSt
    }

    behavior of "SubtypeTypeReplacer"

    it should "replace a subtype using the type" in {
        val sts = Schema.derived[ SuperT ].shape.subtypeDescriptions

        case class SubT4( str : String )

        val newSt = Subtype.builder[ Int, SubT4, String, "str" ]
          .typeName( "test-subtype" )
          .toSuper( _.str.length )
          .fromSuper( v => Some( SubT4( v.toString ) ) )
          .discriminatorValue( "test-value" )
          .fromSchema( Schema.derived )
          .build

        val newSts = SubtypeTypeReplacer.replace( t[ SubT2 ], sts, newSt )
        newSts.size shouldBe 3
        newSts.head.typeName shouldBe "SubT1"
        newSts.tail.head.typeName shouldBe "test-subtype"
        newSts.tail.head shouldBe newSt
    }

}
