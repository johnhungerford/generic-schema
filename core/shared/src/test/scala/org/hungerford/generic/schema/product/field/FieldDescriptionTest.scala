package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers


class FieldDescriptionTest extends AnyFlatSpecLike with Matchers {
    behavior of "FieldTranslator"

    it should "translate a tuple of field descriptions" in {
        case class ID[ T ]()

        val intFd = FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldDescriptionBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldDescriptionBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val intTfd = TranslatedFieldDescription[ Int, ID ]( intFd.fieldName, new ID[ Int ](), intFd.description, intFd.validators )
        val strTfd = TranslatedFieldDescription[ String, ID ]( strFd.fieldName, new ID[ String ](), strFd.description, strFd.validators )
        val boolTfd = TranslatedFieldDescription[ Boolean, ID ]( boolFd.fieldName, new ID[ Boolean ](), boolFd.description, boolFd.validators )

        val fds = intFd *: strFd *: boolFd *: EmptyTuple


        given idFt[ T, S ] : FieldTranslator[ T, S, ID ] = new FieldTranslator[ T, S, ID ] {
            def translate( description : FieldDescription.AuxS[ T, S ] ) : TranslatedFieldDescription[ T, ID ] =
                TranslatedFieldDescription[ T, ID ]( description.fieldName, new ID[ T ](), description.description, description.validators )
        }

        val res = FieldTupleTranslator.translateFieldDescriptions[ FieldDescription.AuxS[Int, Unit] *: FieldDescription.AuxS[String, Unit] *: FieldDescription.AuxS[Boolean, Unit] *: EmptyTuple, ID ]( fds )
        res shouldBe intTfd *: strTfd *: boolTfd *: EmptyTuple
    }

}
