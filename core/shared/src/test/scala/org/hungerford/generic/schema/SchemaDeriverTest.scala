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

    it should "handle a simple recursive case" in {
        case class Recur( recur : Recur )

        val recurSch = Schema.derived[ Recur ]
        import recurSch.givenSchema

        val innerSch = recurSch( "recur" ).schema

        innerSch shouldBe recurSch
    }

    it should "handle a recursive case nested in products" in {
        case class Inner( outer : Outer )
        case class Outer( inner : Inner )

        val outerSch = Schema.derived[ Outer ]
        import outerSch.givenSchema

        val recurSch = outerSch( "inner" / "outer" ).schema

        recurSch shouldBe outerSch
    }

    it should "handle a recursive case nested in coproducts" in {
        sealed trait Coprod
        case object Inner extends Coprod
        case class Recur( coprod : Coprod ) extends Coprod

        val outerSch = Schema.derived[ Coprod ]
        import outerSch.givenSchema

        val recurSch = outerSch( "Recur" / "coprod" ).schema

        recurSch shouldBe outerSch
    }

    it should "handle a recursive case nested in coproducts and products" in {
        case class Outer( coprod : Coprod )
        sealed trait Coprod
        case object Inner extends Coprod
        case class Recur( coprod : Coprod ) extends Coprod

        val outerSch = Schema.derived[ Outer ]
        import outerSch.givenSchema

        val recurSch = outerSch( "coprod" / "Recur" / "coprod" ).schema

        recurSch shouldBe outerSch
    }

    it should "handle recursive cases with multiple different recursed types" in {
        case class Outer( field1 : Int, field2 : Inner1 )
        case class Inner1( field1 : Inner2, field2 : String )
        case class Inner2( field1 : Boolean, field2 : Inner1, field3 : Inner3 )
        sealed trait Inner3
        case object Inner3Sub1 extends Inner3
        case class Inner3Sub2( field1 : Double, field2 : Int, field3 : Inner4 ) extends Inner3
        case class Inner3Sub3( field1 : Inner2 ) extends Inner3
        case class Inner4( field1 : Inner3 )

        val outerSch = Schema.derived[ Outer ]

        val inner2Sch = outerSch( "field2" / "field1" ).schema
        import inner2Sch.givenSchema

        val inner3Sch = inner2Sch( "field3" ).schema
        import inner3Sch.givenSchema

        outerSch( "field2" / "field1" / "field2" ).schema shouldBe outerSch( "field2" ).schema
        inner3Sch( "Inner3Sub3" / "field1" ).schema shouldBe inner2Sch
        inner3Sch( "Inner3Sub2" / "field3" / "field1" ).schema shouldBe inner3Sch
    }
}
