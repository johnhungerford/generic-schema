package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.product.field.{Field, FieldDsl}
import org.hungerford.generic.schema.selector.SelectorDsl

class SchemaDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "static schema dsl"

    case class TestCase( int : Int, str : String )
    object TestSchemaDsl extends SchemaDsl with FieldDsl with SelectorDsl

    it should "allow building schemas" in {
        assertDoesNotCompile( """Schema.primitiveBuilder[ Int ].description( "test-description" ).build""" )
        assertDoesNotCompile( """Schema.productBuilder[ TestCase ]
                                |          .addField( Field.builder[ Int ].fieldName( "int" ).primitive.build )
                                |          .addField( Field.builder[ String ].fieldName( "str" ).primitive.build )
                                |          .construct( (int, str) => TestCase( int, str ) )
                                |          .deconstruct( v => (v.int, v.str) )
                                |          .build""".stripMargin )
        assertDoesNotCompile( """Schema.derivedBuilder[ TestCase ].build""" )
        assertDoesNotCompile( """Schema.derived[ TestCase ]""" )

        import TestSchemaDsl.*

        Schema.primitiveBuilder[ Int ].description( "test-description" ).build
        Schema.productBuilder[ TestCase ]
          .addField( Field.builder[ Int ].fieldName( "int" ).primitive.build )
          .addField( Field.builder[ String ].fieldName( "str" ).primitive.build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( v => (v.int, v.str) )
          .build
        Schema.derivedBuilder[ TestCase ].build
        Schema.derived[ TestCase ]
    }

    it should "allow modifying schemas" in {

        val sch = {
            import TestSchemaDsl.*
            Schema.derived[ TestCase ]
        }

        assertDoesNotCompile( """sch.withDescription( "test-description" )""" )
        assertDoesNotCompile( """sch.withDescription( "test-description" ).withoutDescription""" )
        assertDoesNotCompile( """sch.modifyComponent( "str" )( _.withName( "string_field" ) )""" )
        assertDoesNotCompile( """sch.rebuild.removeField( "int" )
                                |          .construct( str => TestCase( 5, str ) )
                                |          .deconstruct( _.str )
                                |          .build""".stripMargin )

        import TestSchemaDsl.*

        val withDesc = sch.withDescription( "test-description" )
        withDesc.genericDescription shouldBe Some( "test-description" )
        withDesc.withoutDescription.genericDescription shouldBe None

        val updated = sch.modifyComponent( "str" )( _.withName( "string_field" ) )
        updated( "string_field" ).fieldName shouldBe "string_field"

        sch.rebuild.removeField( "int" )
          .construct( str => TestCase( 5, str ) )
          .deconstruct( _.str )
          .build

    }

    sealed trait NestedSuperT
    final case class NSub1( dbl : Double ) extends NestedSuperT
    final case class NSub2( flt : Float ) extends NestedSuperT

    sealed trait SuperT
    final case class SubT1() extends SuperT
    sealed trait SubT2 extends SuperT
    final case class SubT2A( int : Int ) extends SubT2
    final case class SubT2B( nst : NestedSuperT ) extends SubT2

    case class Wrapper( st : SuperT )

    it should "allow retrieval and modification of deeply nested component involving both products and coproducts" in {
        import TestSchemaDsl.*

        val sch = Schema.derived[ Wrapper ]

        val subComponent = sch( "st" / "SubT2" / "SubT2B" / "nst" / "NSub2" / "flt" )
        subComponent.fieldName shouldBe "flt"
        subComponent.schema.shape shouldBe ()
        subComponent.description shouldBe None

        val newSch = sch.modifyComponent( "st" / "SubT2" / "SubT2B" / "nst" / "NSub2" / "flt" )(
            _.rebuild
              .fieldName( "NEW_NAME" )
              .description( "test-description" )
              .build
        )

        assertDoesNotCompile( """newSch( "st" / "SubT2" / "SubT2B" / "nst" / "NSub2" / "flt" )""")
        val newSc = newSch( "st" / "SubT2" / "SubT2B" / "nst" / "NSub2" / "NEW_NAME" )

        newSc.fieldName shouldBe "NEW_NAME"
        newSc.schema.shape shouldBe ()
        newSc.description shouldBe Some( "test-description" )
    }

}
