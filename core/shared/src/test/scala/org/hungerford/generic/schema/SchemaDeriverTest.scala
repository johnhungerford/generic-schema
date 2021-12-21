package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Default.dsl.*

class SchemaDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaDeriver"

    it should "derive a product schema equivalent to one built with implicit primitives and no additional fields" in {
        case class Test( int: Int, str: String )

        import org.hungerford.generic.schema.primitives.Primitives.given

        val testSchema = Schema.productBuilder[ Test ]
          .addField( FieldBuilder[ Int ].fieldName( "int" ).fromSchema.build )
          .addField( FieldBuilder[ String ].fieldName( "str" ).fromSchema.build )
          .construct( ( int, str ) => Test( int, str ) )
          .deconstruct( value => (value.int, value.str) )
          .build

        val newTestRes = SchemaDeriver.schema[ Test ]

        newTestRes.shape.fieldDescriptions shouldBe testSchema.shape.fieldDescriptions

    }

    it should "derive a product schema for a large case class" in {
        case class Test( int: Int, str: String, dbl: Double, bool: Boolean, newInt: Int )

        import org.hungerford.generic.schema.primitives.Primitives.given

        val newTestRes = SchemaDeriver.schema[ Test ]

        newTestRes.shape.size shouldBe 5
    }

    it should "derive a coproduct schema" in {
        sealed trait SuperT
        case class SubT1( int : Int ) extends SuperT
        case class SubT2( str : String ) extends SuperT

        val expectedSchema = Schema.coproductBuilder[ SuperT ]
          .buildSubtype[ SubT1 ]( _.typeName( "SubT1" ).fromSchema( Schema.derived ).build )
          .buildSubtype[ SubT2 ]( _.typeName( "SubT2" ).fromSchema( Schema.derived ).build )
          .build

        val sch = SchemaDeriver.schema[ SuperT ]
        sch.shape.subtypeDescriptions.size shouldBe 2

        val schStDescripts = sch.shape.subtypeDescriptions
        val expStDescripts = expectedSchema.shape.subtypeDescriptions
        schStDescripts.head.typeName shouldBe expStDescripts.head.typeName
        schStDescripts.head.schema.shape.fieldDescriptions shouldBe expStDescripts.head.schema.shape.fieldDescriptions
        schStDescripts.tail.head.typeName shouldBe expStDescripts.tail.head.typeName
        schStDescripts.tail.head.schema.shape.fieldDescriptions shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions
    }
}
