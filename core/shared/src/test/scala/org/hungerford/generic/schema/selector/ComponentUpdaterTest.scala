package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.SchemaBuilder
import org.hungerford.generic.schema.product.field.FieldDescriptionBuilder
import org.scalatest.flatspec.AnyFlatSpecLike

class ComponentUpdaterTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ComponentUpdater"

    case class One( str : String )
    val oneSch = SchemaBuilder[ One ].caseClass.build
    case class Two( one : One )
    val twoSch = SchemaBuilder[ Two ].caseClass.build

    it should "update a schema's field using a selector" in {
        val newSch = ComponentUpdater.update( oneSch )( Selector.field( "str" ) ){ field =>
            FieldDescriptionBuilder.from( field )
              .fieldName( "string_field" )
              .build
        }

        newSch.shape.fieldNames shouldBe Set( "string_field" )
    }

    it should "update a schema's nested field using a selector" in {
        val newSch = ComponentUpdater.update( twoSch )( Selector.field( "one" ) / "str" ){ field =>
            FieldDescriptionBuilder.from( field )
              .fieldName( "string_field_2" )
              .build
        }

        newSch.shape.fieldNames shouldBe Set( "one" )
        newSch.shape.fieldDescriptions.head.schema.shape.fieldNames shouldBe Set( "string_field_2" )
    }

}
