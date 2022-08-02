package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema

import org.hungerford.generic.schema.Default.dsl.*

class FieldBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "FieldDescriptionBuilder"

    it should "be able to build a field description using a primitive" in {
        val field = FieldBuilder[ Double, Int ]
          .name( "name" )
          .extractor( _.toInt )
          .primitive
          .build

        field.fieldName shouldBe "name"
        field.schema.shape shouldBe ()
        field.schema.genericDescription.isEmpty shouldBe true
        field.schema.genericValidators.isEmpty shouldBe true
        field.extractor( 25D ) shouldBe 25
    }

    it should "be able to build a field description using given schemas" in {
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        val field = FieldBuilder[ String, Int ]
          .name( "name" )
          .extractor( _.toInt )
          .fromSchema
          .build

        field.fieldName shouldBe "name"
        field.schema.shape shouldBe ()
        field.schema.genericDescription.isEmpty shouldBe false
        field.schema.genericValidators.isEmpty shouldBe true
        field.extractor( "25" ) shouldBe 25
    }

    it should "be able to build a field description by building a schema from scratch" in {
        case class TestClass( intField : Int, strField : String )

        val field = FieldBuilder[ String, TestClass ]
          .name( "test_class" )
          .extractor( v => TestClass( v.toInt, v ) )
          .fromSchema( Schema.productBuilder[ TestClass ]
            .description( "generic-description" )
            .addField( FieldBuilder[ TestClass, Int ].name( "int" ).extractor( _.intField ).primitive.build )
            .addField( FieldBuilder[ TestClass, String ].name( "str" ).extractor( _.strField ).primitive.build )
            .construct( (int, str) => TestClass( int, str ) )
            .build
          )
          .description( "test-description" )
          .build

        field.fieldName shouldBe "test_class"
        field.extractor( "25" ) shouldBe TestClass( 25, "25" )
        field.description shouldBe Some( "test-description" )
        field.validators.isEmpty shouldBe true
        field.schema.genericDescription shouldBe Some( "generic-description" )
        field.schema.shape.fieldDescriptions.size shouldBe 2
        field.schema.shape.fieldDescriptions.head.fieldName shouldBe "int"
        field.schema.shape.fieldDescriptions.tail.head.fieldName shouldBe "str"
    }

    it should "be able to rebuild a field description and update data without erasing" in {
        val originalField = FieldBuilder[ Int, Int ]
          .extractor( v => v )
          .name( "name" )
          .primitive
          .description( "old-description" )
          .build

        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        val newField = FieldBuilder.from( originalField )
          .name( "new_name" )
          .fromSchema
          .build

        newField.fieldName shouldBe "new_name"
        newField.description shouldBe Some( "old-description" )
        newField.extractor( 25 ) shouldBe 25
        newField.validators.isEmpty shouldBe true
        newField.schema.genericDescription.isEmpty shouldBe false
    }

}
