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

        val res = FieldTranslator.translateFieldDescriptions[ FieldDescription.Aux[Int, String, Unit] *: FieldDescription.Aux[String, String, Unit] *: FieldDescription.Aux[Boolean, String, Unit] *: EmptyTuple, ID ]( fds )
        res shouldBe intTfd *: strTfd *: boolTfd *: EmptyTuple
    }

    behavior of "FieldExtractor"

    it should "extract from field descriptions" in {
        val intFd = FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldDescriptionBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldDescriptionBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val fds = intFd *: strFd *: boolFd *: EmptyTuple

        import org.hungerford.generic.schema.types.SimpleExtractor

        given extractor[ T, S ] : SimpleExtractor.Aux[ Map[ String, String ], FieldDescription.AuxS[ T, S ], String ] = {
            new SimpleExtractor[ Map[ String, String ],  FieldDescription.AuxS[ T, S ] ] {
                type Out = String

                def extract( from : Map[ String, String ], informedBy : FieldDescription.AuxS[ T, S ] ) : String = {
                    from( informedBy.fieldName )
                }
            }
        }

        val source = Map( "int" -> "INTEGER FIELD", "str" -> "STRING FIELD", "bool" -> "BOOLEAN FIELD" )


        import org.hungerford.generic.schema.product.field.FieldExtractor

        FieldExtractor.extractFromFieldDescriptions( source, fds ) shouldBe "INTEGER FIELD" *: "STRING FIELD" *: "BOOLEAN FIELD" *: EmptyTuple

    }


}
