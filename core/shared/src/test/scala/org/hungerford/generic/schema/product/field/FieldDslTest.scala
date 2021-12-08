package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.{Schema, Primitive, SchemaDsl}
import org.hungerford.generic.schema.selector.SelectorDsl
import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.product.field.Field

class FieldDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "static field extensions"

    object TestFieldDsl extends FieldDsl with SchemaDsl with SelectorDsl

    it should "allow building a field description" in {
        import TestFieldDsl.*

        Field.primitive[ Int ]( "test-name" ) shouldBe FieldCase[ Int, "test-name", Unit ]( "test-name", Primitive[ Int ]() )
        Field.fromSchema[ Int ]( "test-name" )( using Primitive[ Int ]() ) shouldBe FieldCase[ Int, "test-name", Unit ]( "test-name", Primitive[ Int ]() )
        {
            import org.hungerford.generic.schema.primitives.Primitives.given

            Field.fromSchema[ Int ]( "test-name" ) shouldBe FieldCase[ Int, "test-name", Unit ]( "test-name", Primitive[ Int ]( Some( "Integer number between -2147483648 and 2147483647" ) ) )
            Field.builder[ Int ].fieldName( "test-name" ).fromSchema.build shouldBe FieldCase[ Int, "test-name", Unit ]( "test-name", Primitive[ Int ]( Some( "Integer number between -2147483648 and 2147483647" ) ) )
        }

        case class TC( int : Int , str : String )
        val sch = Schema.derived[ TC ]
        import sch.givenSchema

        Field.fromSchema[ TC ]( "test-name" )
    }

}
