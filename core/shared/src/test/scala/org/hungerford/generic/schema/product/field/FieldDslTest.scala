package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.{Schema, Primitive, SchemaDsl}
import org.hungerford.generic.schema.selector.SelectorDsl
import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.product.field.Field

class FieldDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "static field extensions"

    object TestFieldDsl extends FieldDsl with SchemaDsl with SelectorDsl

    case class TC( int : Int , str : String )

    it should "allow building a field description" in {
        // Haven't imported dsl
        assertDoesNotCompile( """Field.primitive[ Int, Int ]( "test-name", v => v )""" )
        assertDoesNotCompile( """Field.fromSchema[ Int, Int ]( "test-name", v => v )( using Primitive[ Int ]() )""" )
        assertDoesNotCompile( """Schema.derived[ TC ]""" )

        import TestFieldDsl.*

        val fn : Int => Int = v => v

        Field.primitive[ Int, Int ]( "test-name", fn ) shouldBe FieldCase[ Int, Int, "test-name", Unit ]( "test-name", fn, Primitive[ Int ]() )
        Field.fromSchema[ Int, Int ]( "test-name", fn )( using Primitive[ Int ]() ) shouldBe FieldCase[ Int, Int, "test-name", Unit ]( "test-name", fn, Primitive[ Int ]() )
        {
            import org.hungerford.generic.schema.primitives.Primitives.given

            Field.fromSchema[ Int, Int ]( "test-name", fn ) shouldBe FieldCase[ Int, Int, "test-name", Unit ]( "test-name", fn, Primitive[ Int ]( None, Some( "Integer number between -2147483648 and 2147483647" ) ) )
            Field.builder[ Int, Int ].name( "test-name" ).extractor( fn ).fromSchema.build shouldBe FieldCase[ Int, Int, "test-name", Unit ]( "test-name", fn, Primitive[ Int ]( None, Some( "Integer number between -2147483648 and 2147483647" ) ) )
        }

        val sch = Schema.derived[ TC ]
        import sch.givenSchema

        Field.fromSchema[ Int, TC ]( "test-name", v => TC( v, v.toString ) )
    }

    it should "allow modifying a field description" in {
        val field = {
            import TestFieldDsl.{*, given}

            Field.fromSchema[ TC, TC ]( "test-name", v => v )( using Schema.derived[ TC ] )
        }

        assertDoesNotCompile( """field.withDescription( "test-description" )""" )
        assertDoesNotCompile( """field.withName( "new-name" )""" )

        import TestFieldDsl.{*, given}

        field.withDescription( "test-description" ).description shouldBe Some( "test-description" )
        field.withName( "new-name" ).fieldName shouldBe "new-name"
        field.rebuildSchema( _.rebuildField( "int" )( _.name( "int_field" ).build ).build )
    }

}
