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

    behavior of "FieldInjector"

    it should "inject fields into an object" in {

        val intFd = FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldDescriptionBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldDescriptionBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val fds = intFd *: strFd *: boolFd *: EmptyTuple

        given [ T, S ] : FieldInjector[ T, S, Map[ String, String ] ] = {
            new FieldInjector[ T, S, Map[ String, String ] ] {
                def inject( field : FieldDescription.AuxS[ T, S ], value : T, into : Map[ String, String ] ) : Map[ String, String ] = {
                    into + (field.fieldName -> value.toString)
                }
            }
        }

        val injectionResults = FieldInjector.inject( fds, 3 *: "hello" *: false *: EmptyTuple, Map.empty[ String, String ] )
        
        injectionResults shouldBe Map( "int" -> "3", "str" -> "hello", "bool" -> "false" )

    }

    behavior of "TranslatedFieldInjector"

    it should "inject fields into an object" in {
        case class ID[ T ]()

        val intFd = FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldDescriptionBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldDescriptionBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val intTfd = TranslatedFieldDescription[ Int, ID ]( intFd.fieldName, new ID[ Int ](), intFd.description, intFd.validators )
        val strTfd = TranslatedFieldDescription[ String, ID ]( strFd.fieldName, new ID[ String ](), strFd.description, strFd.validators )
        val boolTfd = TranslatedFieldDescription[ Boolean, ID ]( boolFd.fieldName, new ID[ Boolean ](), boolFd.description, boolFd.validators )

        val fds = intFd *: strFd *: boolFd *: EmptyTuple
        val tfds = intTfd *: strTfd *: boolTfd *: EmptyTuple

        given idFt[ T, S ] : FieldTranslator[ T, S, ID ] = new FieldTranslator[ T, S, ID ] {
            def translate( description : FieldDescription.AuxS[ T, S ] ) : TranslatedFieldDescription[ T, ID ] =
                TranslatedFieldDescription[ T, ID ]( description.fieldName, new ID[ T ](), description.description, description.validators )
        }

        given [ T ] : TranslatedFieldInjector[ T, Map[ String, String ], ID ] = {
            new TranslatedFieldInjector[ T, Map[ String, String ], ID ] {
                def inject( field : TranslatedFieldDescription[ T, ID ], value : T, into : Map[ String, String ] ) : Map[ String, String ] = {
                    into + (field.fieldName -> value.toString)
                }
            }
        }

        val injectionResults1 = TranslatedFieldInjector.inject[ FieldDescription.AuxS[ Int, Unit ] *: FieldDescription.AuxS[ String, Unit ] *: FieldDescription.AuxS[ Boolean, Unit ] *: EmptyTuple, Int *: String *: Boolean *: EmptyTuple, Map[ String, String ], ID ]( fds, 3 *: "hello" *: false *: EmptyTuple, Map.empty[ String, String ] )
        val injectionResults2 = TranslatedFieldInjector.inject[ TranslatedFieldDescription[ Int, ID ] *: TranslatedFieldDescription[ String, ID ] *: TranslatedFieldDescription[ Boolean, ID ] *: EmptyTuple, Int *: String *: Boolean *: EmptyTuple, Map[ String, String ], ID ]( tfds, 3 *: "hello" *: false *: EmptyTuple, Map.empty[ String, String ] )

        injectionResults1 shouldBe Map( "int" -> "3", "str" -> "hello", "bool" -> "false" )

    }


}
