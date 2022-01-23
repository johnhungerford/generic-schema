package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*

class FieldDescriptionTest extends AnyFlatSpecLike with Matchers {
    behavior of "FieldTranslator"

    it should "translate a tuple of field descriptions" in {
        case class ID[ T ]()

        val intFd = FieldBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val intTfd = TranslatedFieldDescription[ Int, ID ]( intFd.fieldName, new ID[ Int ](), intFd.description, intFd.validators )
        val strTfd = TranslatedFieldDescription[ String, ID ]( strFd.fieldName, new ID[ String ](), strFd.description, strFd.validators )
        val boolTfd = TranslatedFieldDescription[ Boolean, ID ]( boolFd.fieldName, new ID[ Boolean ](), boolFd.description, boolFd.validators )

        val fds = intFd *: strFd *: boolFd *: EmptyTuple


        given idFt[ T, F, N <: FieldName, S ] : FieldTranslator[ T, F, N, S, ID ] = new FieldTranslator[ T, N, S, ID ] {
            def translate( description : Field.Aux[ T, F, N, S ] ) : TranslatedFieldDescription[ T, ID ] =
                TranslatedFieldDescription[ T, ID ]( description.fieldName, new ID[ T ](), description.description, description.validators )
        }

        val res = FieldTupleTranslator.translateFieldDescriptions( fds )
        res shouldBe intTfd *: strTfd *: boolTfd *: EmptyTuple
    }

    behavior of "FieldDescriptionBuilder"

    it should "be able to rebuild a schema before field name has been set" in {
        case class Inner( int : Int )
        case class Outer( inner : Inner )

        val fd = FieldBuilder[ Outer ]
          .fromSchema( Schema.derived )
          .rebuildSchema(
              _.rebuildField( "inner" )(
                  _.fieldName( "inner_field" )
                    .build
              ).build
          )
          .fieldName( "outer_field" )
          .build

        fd.fieldName shouldBe "outer_field"
        fd.schema.shape.fieldDescriptions.size shouldBe 1
        fd.schema.shape.fieldDescriptions.head.fieldName shouldBe "inner_field"
    }

}
