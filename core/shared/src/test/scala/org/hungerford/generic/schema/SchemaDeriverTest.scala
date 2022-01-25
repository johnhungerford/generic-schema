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
          .addField( FieldBuilder[ Test, Int ].name( "int" ).extractor( _.int ).fromSchema.build )
          .addField( FieldBuilder[ Test, String ].name( "str" ).extractor( _.str ).fromSchema.build )
          .construct( ( int, str ) => Test( int, str ) )
          .build

        val newTestRes = SchemaDeriver.schema[ Test ]

        newTestRes.shape.fieldDescriptions.size shouldBe testSchema.shape.fieldDescriptions.size
        newTestRes.shape.fieldDescriptions.head.fieldName shouldBe testSchema.shape.fieldDescriptions.head.fieldName
        newTestRes.shape.fieldDescriptions.head.description shouldBe testSchema.shape.fieldDescriptions.head.description
        newTestRes.shape.fieldDescriptions.head.extractor( Test( 5, "hello" ) ) shouldBe testSchema.shape.fieldDescriptions.head.extractor( Test( 5, "hello" ) )
        newTestRes.shape.fieldDescriptions.tail.head.fieldName shouldBe testSchema.shape.fieldDescriptions.tail.head.fieldName
        newTestRes.shape.fieldDescriptions.tail.head.description shouldBe testSchema.shape.fieldDescriptions.tail.head.description
        newTestRes.shape.fieldDescriptions.tail.head.extractor( Test( 5, "hello" ) ) shouldBe testSchema.shape.fieldDescriptions.tail.head.extractor( Test( 5, "hello" ) )


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
          .buildSubtype[ SubT1 ]( _.typeName( "SubT1" ).fromSchema( Schema.derived ).fromSuper( { case v@SubT1( _ ) => Some( v ); case _ => None } ).build )
          .buildSubtype[ SubT2 ]( _.typeName( "SubT2" ).fromSchema( Schema.derived ).fromSuper( { case v@SubT2( _ ) => Some( v ); case _ => None } ).build )
          .build

        val sch = SchemaDeriver.schema[ SuperT ]
        sch.shape.subtypeDescriptions.size shouldBe 2

        val schStDescripts = sch.shape.subtypeDescriptions
        val expStDescripts = expectedSchema.shape.subtypeDescriptions
        schStDescripts.head.typeName shouldBe expStDescripts.head.typeName

        schStDescripts.head.schema.shape.fieldDescriptions.size shouldBe expStDescripts.head.schema.shape.fieldDescriptions.size
        schStDescripts.head.schema.shape.fieldDescriptions.head.fieldName shouldBe expStDescripts.head.schema.shape.fieldDescriptions.head.fieldName
        schStDescripts.head.schema.shape.fieldDescriptions.head.extractor( SubT1( 5 ) ) shouldBe expStDescripts.head.schema.shape.fieldDescriptions.head.extractor( SubT1( 5 ) )
        schStDescripts.head.schema.shape.fieldDescriptions.head.description shouldBe expStDescripts.head.schema.shape.fieldDescriptions.head.description
        schStDescripts.head.schema.shape.fieldDescriptions.tail shouldBe expStDescripts.head.schema.shape.fieldDescriptions.tail

        schStDescripts.tail.head.typeName shouldBe expStDescripts.tail.head.typeName

        schStDescripts.tail.head.schema.shape.fieldDescriptions.size shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions.size
        schStDescripts.tail.head.schema.shape.fieldDescriptions.head.fieldName shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions.head.fieldName
        schStDescripts.tail.head.schema.shape.fieldDescriptions.head.extractor( SubT2( "hello" ) ) shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions.head.extractor( SubT2( "hello" ) )
        schStDescripts.tail.head.schema.shape.fieldDescriptions.head.description shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions.head.description
        schStDescripts.tail.head.schema.shape.fieldDescriptions.tail shouldBe expStDescripts.tail.head.schema.shape.fieldDescriptions.tail
    }
}
