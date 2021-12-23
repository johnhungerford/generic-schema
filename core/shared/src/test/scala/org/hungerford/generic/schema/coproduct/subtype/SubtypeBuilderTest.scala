package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.product.field.FieldBuilder
import org.hungerford.generic.schema.{Primitive, SchemaDeriver}
import org.hungerford.generic.schema.validator.Validator
import org.scalatest.flatspec.AnyFlatSpecLike

class SubtypeBuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SubtypeBuilder.empty"

    val testBuilder = SubtypeBuilder.empty[ TestType, Int, Unit, Nothing ]

    trait TestType

    it should "be able to add a typename, and not be able to build" in {
        testBuilder.tn shouldBe ()

        val builder = testBuilder
          .typeName( "test-name" )

        builder.tn shouldBe "test-name"

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to add a schema from a schema, and not be able to build" in {
        testBuilder.sch shouldBe ()

        val builder = testBuilder
          .fromSchema( Primitive[ Int ]() )

        builder.sch shouldBe Primitive[ Int ]()

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to add a simple primitive schema, and not be able to build" in {
        testBuilder.sch shouldBe ()

        val builder = testBuilder
          .primitive

        builder.sch shouldBe Primitive[ Int ]()

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to add an asSuper function, and not be able to build" in {
        testBuilder.ts shouldBe ()

        val builder = testBuilder
          .toSuper( v => new TestType { val i : Int = v } )

        val res = builder.ts( 5 )

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to add a discriminator value, and not be able to build" in {
        case class TestProd( int : Int, str : String )

        val tb = SubtypeBuilder.empty[ TestType, TestProd, String, "str" ]

        tb.dv shouldBe ()

        val builder = tb.discriminatorValue( "hello" )

        builder.dv shouldBe "hello"
        implicitly[builder.dv.type =:= "hello"]

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to add a validators, and not be able to build" in {
        testBuilder.vals shouldBe Set.empty

        val builder = testBuilder
          .validate( Validator.min( -5 ) )

        builder.vals.size shouldBe 1
        builder.validate( Validator.max( 20 ), Validator.nonZero )
          .vals.size shouldBe 3

        assertDoesNotCompile( """builder.build""")
    }

    it should "be able to build if you add a typename and schema if it an actual subtype of T and has no discriminator type" in {
        trait SuperT
        case class SubT(int: Int) extends SuperT

        val st = SubtypeBuilder.empty[ SuperT, SubT, Unit, Nothing ]
          .typeName( "name" )
          .primitive
          .build

        st.typeName shouldBe "name"
        st.toSuper( SubT( 5 ) ) shouldBe SubT( 5 )
    }

    it should "be able to build if you add a typename and schema and asSuper if has no discriminator type and is not an actual subtype of T" in {
        case class SuperC(int: Int)
        case class SubC(int: Int)

        val b1 = SubtypeBuilder.empty[ SuperC, SubC, Unit, Nothing ]
          .typeName( "name" )
          .primitive

        assertDoesNotCompile("""b1.build""")

        val st = b1.toSuper( v => SuperC( v.int ) ).fromSuper( v => Some( SubC( v.int ) ) ).build

        st.typeName shouldBe "name"
        st.schema shouldBe Primitive[ Int ]()
        st.toSuper( SubC( 5 ) ) shouldBe SuperC( 5 )
    }

    it should "be able to build if you add a typename and schema and asSuper and a discriminator value if it has a discriminator type and is not an actual subtype of T" in {
        case class SuperC(int: Int)
        case class SubC(int: Int)

        val subcSch = ProductSchemaBuilder[ SubC ]
          .addField( FieldBuilder[ Int ].fieldName( "int" ).primitive.build )
          .construct( SubC.apply )
          .deconstruct( _.int )
          .build

        val b1 = SubtypeBuilder.empty[ SuperC, SubC, Int, "int" ]
          .typeName( "name" )
          .fromSchema( subcSch )
          .discriminatorValue( 500 )

        val bdv = b1.dv

        assertDoesNotCompile("""b1.build""")

        val st = b1.toSuper( v => SuperC( v.int ) ).fromSuper( v => Some( SubC( v.int ) ) ).build

        st.typeName shouldBe "name"
        st.schema shouldBe subcSch
        st.toSuper( SubC( 5 ) ) shouldBe SuperC( 5 )
        st.discriminatorValue shouldBe 500
    }

}
