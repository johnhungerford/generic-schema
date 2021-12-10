package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.field.FieldBuilder
import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*

class ComponentUpdaterTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ComponentUpdater"

    case class One( str : String )
    val oneSch = Schema.derived[ One ]
    case class Two( one : One )
    val twoSch = Schema.derived[ Two ]
    case class Three( two : Two, str : String )
    case class Four( int : Int, three : Three )
    case class Five( dbl : Double, map : Map[ Int, String ], four : Four )
    val fiveSch = Schema.derived[ Five ]

    it should "update a schema's field using a selector" in {
        val newSch = ComponentUpdater.update( oneSch )( Selector.field( "str" ) ){ field =>
            FieldBuilder.from( field )
              .fieldName( "string_field" )
              .build
        }

        newSch.shape.fieldNames shouldBe Set( "string_field" )
    }

    it should "update a schema's nested field using a selector" in {
        val newSch = ComponentUpdater.update( twoSch )( Selector.field( "one" ) / "str" ){ field =>
            FieldBuilder.from( field )
              .fieldName( "string_field_2" )
              .build
        }

        newSch.shape.fieldNames shouldBe Set( "one" )
        newSch.shape.fieldDescriptions.head.schema.shape.fieldNames shouldBe Set( "string_field_2" )
    }

    it should "update a highly nested field using a selector" in {
        val newSch = ComponentUpdater.update( fiveSch )( Selector.field( "four" ) / "three" / "two" / "one" / "str" ) { field =>
            FieldBuilder.from( field )
              .fieldName( "string_field_5" )
              .build
        }

        val fourS = newSch.shape.fieldDescriptions.tail.tail.head.schema
        val threeS = fourS.shape.fieldDescriptions.tail.head.schema
        val twoS = threeS.shape.fieldDescriptions.head.schema
        val oneS = twoS.shape.fieldDescriptions.head.schema
        oneS.shape.fieldDescriptions.head.fieldName shouldBe "string_field_5"
    }

    it should "update a highly nest field using an ambiguous selector" in {
        import Selector.given
        val newSch = ComponentUpdater.update( fiveSch )( "four" / "three" / "two" / "one" / "str" ) { field =>
            FieldBuilder.from( field )
              .fieldName( "string_field_5" )
              .build
        }

        val fourS = newSch.shape.fieldDescriptions.tail.tail.head.schema
        val threeS = fourS.shape.fieldDescriptions.tail.head.schema
        val twoS = threeS.shape.fieldDescriptions.head.schema
        val oneS = twoS.shape.fieldDescriptions.head.schema
        oneS.shape.fieldDescriptions.head.fieldName shouldBe "string_field_5"
    }

}
