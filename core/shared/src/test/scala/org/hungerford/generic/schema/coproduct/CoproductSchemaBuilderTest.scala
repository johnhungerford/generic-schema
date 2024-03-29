package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{Schema, Primitive}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike

import generic.schema.exports.*

class CoproductSchemaBuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "CoproductSchemaBuilder"

    case class SomeTrait( value : Any )

    it should "be able to add a subtype" in {
        val csb = Schema.coproductBuilder[ SomeTrait ]
          .buildSubtype[ Int ]( _.typeName( "int" ).primitive.toSuper( SomeTrait.apply ).fromSuper( v => v.value match { case i : Int => Some( i ); case _ => None } ).build )
          .buildSubtype[ String ]( _.typeName( "str" ).primitive.toSuper( SomeTrait.apply ).fromSuper( v => v.value match { case _ : Int => None; case other => Some( other.toString ) } ).build )
          .build

        summon[ csb.type <:< Schema.Aux[ SomeTrait, CoproductShape[ SomeTrait, (Subtype[ SomeTrait, Int, Unit, Unit, Unit, "int",  Unit ], Subtype[ SomeTrait, String, Unit, Unit, Unit, "str", Unit ]), (Int, String), Unit, Unit ] ] ]
    }

    sealed trait SuperTrait
    case class SubCase( int : Int ) extends SuperTrait

    it should "be able to add real subtype" in {
        val csb = Schema.coproductBuilder[ SuperTrait ]
          .buildSubtype[ SubCase ]( _.typeName( "sub-case" ).fromSchema( Schema.derived ).fromSuper( { case v@SubCase(_) => Some( v ); case _ => None } ).build )
          .build
    }

    case class SubCase2( int : Int ) extends SuperTrait

    it should "not be able to add two subtypes with the same name" in {
        val csb = Schema.coproductBuilder[ SuperTrait ]
          .buildSubtype[ SubCase ]( _.typeName( "sub-case" ).fromSchema( Schema.derived ).fromSuper( { case v@SubCase(_) => Some( v ); case _ => None } ).build )

        assertDoesNotCompile(
            """csb.buildSubtype[ SubCase2 ]( _.typeName( "sub-case" )
              |.fromSchema( Schema.derived ).fromSuper( { case v@SubCase2(_) => Some( v ); case _ => None } ).build )""".stripMargin,
        )

        assertCompiles(
            """csb.buildSubtype[ SubCase2 ]( _.typeName( "sub-case-2" )
              |.fromSchema( Schema.derived ).fromSuper( { case v@SubCase2(_) => Some( v ); case _ => None } ).build )""".stripMargin,
        )
    }

    it should "not be able to add two subtypes with same discriminator value" in {
        val csb = Schema.coproductBuilder[ SuperTrait ]
          .discriminator[ Int ]( "int" )
          .buildSubtype[ SubCase ]( _.typeName( "sub-case" ).fromSchema( Schema.derived ).fromSuper( { case v@SubCase(_) => Some( v ); case _ => None } ).discriminatorValue( 1 ).build )

        assertDoesNotCompile(
            """csb.buildSubtype[ SubCase2 ]( _.typeName( "sub-case-2" ).discriminatorValue( 1 )
              |.fromSchema( Schema.derived ).fromSuper( { case v@SubCase2(_) => Some( v ); case _ => None } ).build )""".stripMargin,
        )

        assertCompiles(
            """csb.buildSubtype[ SubCase2 ]( _.typeName( "sub-case-2" ).discriminatorValue( 2 )
              |.fromSchema( Schema.derived ).fromSuper( { case v@SubCase2(_) => Some( v ); case _ => None } ).build )""".stripMargin,
        )

    }

    it should "be able to remove a subfield by name" in {
        val csb = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( "SubCase" )
          .build

        csb.shape.subtypeDescriptions.size shouldBe 1
        csb.shape.subtypeDescriptions.head.typeName shouldBe "SubCase2"

        val csb2 = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( "SubCase2" )
          .build

        csb2.shape.subtypeDescriptions.size shouldBe 1
        csb2.shape.subtypeDescriptions.head.typeName shouldBe "SubCase"
    }

    it should "be able to remove a subfield by index" in {
        val csb = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( 0 )
          .build

        csb.shape.subtypeDescriptions.size shouldBe 1
        csb.shape.subtypeDescriptions.head.typeName shouldBe "SubCase2"

        val csb2 = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( 1 )
          .build

        csb2.shape.subtypeDescriptions.size shouldBe 1
        csb2.shape.subtypeDescriptions.head.typeName shouldBe "SubCase"
    }

    it should "be able to remove a subfield by type" in {
        val csb = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( t[SubCase] )
          .build

        csb.shape.subtypeDescriptions.size shouldBe 1
        csb.shape.subtypeDescriptions.head.typeName shouldBe "SubCase2"

        val csb2 = Schema.derivedBuilder[ SuperTrait ]
          .removeSubtype( t[SubCase2] )
          .build

        csb2.shape.subtypeDescriptions.size shouldBe 1
        csb2.shape.subtypeDescriptions.head.typeName shouldBe "SubCase"
    }

    it should "be able to modify a subfield" in {
        val sch = Schema.derivedBuilder[ SuperTrait ]
          .modifySubtype( "SubCase" )( v =>
              SubtypeBuilder.from( v )
                .typeName( "NEW-NAME" )
                .build
          ).build

        sch( "NEW-NAME" / "int" ).schema.shape shouldBe ()
    }

}
