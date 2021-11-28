package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.SchemaBuilder

class FieldDescriptionBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "FieldDescriptionBuilder"

    it should "be able to build a field description using a primitive" in {
        val field = FieldDescriptionBuilder[ Int ]
          .fieldName( "name" )
          .primitive
          .build

        field.fieldName shouldBe "name"
        field.schema.shape shouldBe ()
        field.schema.genericDescription.isEmpty shouldBe true
        field.schema.genericValidators.isEmpty shouldBe true
    }

    it should "be able to build a field description using given schemas" in {
        import org.hungerford.generic.schema.primitives.Primitives.given

        val field = FieldDescriptionBuilder[ Int ]
          .fieldName( "name" )
          .fromSchema
          .build

        field.fieldName shouldBe "name"
        field.schema.shape shouldBe ()
        field.schema.genericDescription.isEmpty shouldBe false
        field.schema.genericValidators.isEmpty shouldBe true
    }

    it should "be able to build a field description by building a schema from scratch" in {
        case class TestClass( intField : Int, strField : String )

        val field = FieldDescriptionBuilder[ TestClass ]
          .fieldName( "test_class" )
          .buildSchema( _.product
            .description( "generic-description" )
            .addField( FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build )
            .addField( FieldDescriptionBuilder[ String ].fieldName( "str" ).primitive.build )
            .construct( (tup, _) => { val (int, str) = tup; TestClass( int, str ) } )
            .deconstruct( value => ((value.intField, value.strField), Map.empty) )
            .build
          )
          .description( "test-description" )
          .build

        field.fieldName shouldBe "test_class"
        field.description shouldBe Some( "test-description" )
        field.validators.isEmpty shouldBe true
        field.schema.genericDescription shouldBe Some( "generic-description" )
        field.schema.shape.fieldDescriptions.size shouldBe 2
        field.schema.shape.fieldDescriptions.head.fieldName shouldBe "int"
        field.schema.shape.fieldDescriptions.tail.head.fieldName shouldBe "str"
    }

    it should "be able to rebuild a field description and update data without erasing" in {
        val originalField = FieldDescriptionBuilder[ Int ]
          .fieldName( "name" )
          .primitive
          .description( "old-description" )
          .build

        import org.hungerford.generic.schema.primitives.Primitives.given

        val newField = FieldDescriptionBuilder.from( originalField )
          .fieldName( "new_name" )
          .fromSchema
          .build

        newField.fieldName shouldBe "new_name"
        newField.description shouldBe Some( "old-description" )
        newField.validators.isEmpty shouldBe true
        newField.schema.genericDescription.isEmpty shouldBe false
    }

}
