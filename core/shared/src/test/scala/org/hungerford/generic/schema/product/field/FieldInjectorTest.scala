package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.validator.Validator

class FieldInjectorTest extends AnyFlatSpecLike with Matchers {

    case class ID[ T ]()

    behavior of "FieldInjector"

    it should "inject fields into an object" in {

        val intFd = FieldBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val fds = intFd *: strFd *: boolFd *: EmptyTuple

        given [ T, N <: FieldName, S ] : FieldInjector[ T, N, S, Map[ String, String ] ] = {
            new FieldInjector[ T, N, S, Map[ String, String ] ] {
                def inject( field : Field.Aux[ T, N, S ], value : T, into : Map[ String, String ] ) : Map[ String, String ] = {
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

        val intFd = FieldBuilder[ Int ].fieldName( "int" ).primitive.build
        val strFd = FieldBuilder[ String ].fieldName( "str" ).primitive.build
        val boolFd = FieldBuilder[ Boolean ].fieldName( "bool" ).primitive.build

        val intTfd = TranslatedFieldDescription[ Int, ID ]( intFd.fieldName, new ID[ Int ](), intFd.description, intFd.validators )
        val strTfd = TranslatedFieldDescription[ String, ID ]( strFd.fieldName, new ID[ String ](), strFd.description, strFd.validators )
        val boolTfd = TranslatedFieldDescription[ Boolean, ID ]( boolFd.fieldName, new ID[ Boolean ](), boolFd.description, boolFd.validators )

        val fds = intFd *: strFd *: boolFd *: EmptyTuple
        val tfds = intTfd *: strTfd *: boolTfd *: EmptyTuple

        given idFt[ T, N <: FieldName, S ] : FieldTranslator[ T, N, S, ID ] = new FieldTranslator[ T, N, S, ID ] {
            def translate( description : Field.Aux[ T, N, S ] ) : TranslatedFieldDescription[ T, ID ] =
                TranslatedFieldDescription[ T, ID ]( description.fieldName, new ID[ T ](), description.description, description.validators )
        }

        given [ T ] : TranslatedFieldInjector[ T, Map[ String, String ], ID ] = {
            new TranslatedFieldInjector[ T, Map[ String, String ], ID ] {
                def inject( field : TranslatedFieldDescription[ T, ID ], value : T, into : Map[ String, String ] ) : Map[ String, String ] = {
                    into + (field.fieldName -> value.toString)
                }
            }
        }

        type R = TranslatedFieldDescription[ Int, ID ] *: TranslatedFieldDescription[ String, ID ] *: TranslatedFieldDescription[ Boolean, ID ] *: EmptyTuple
        type RV = Int *: String *: Boolean *: EmptyTuple

        val injectionResults = TranslatedFieldInjector.inject[ R, RV, Map[ String, String ], ID ]( tfds, 3 *: "hello" *: false *: EmptyTuple, Map.empty[ String, String ] )

        injectionResults shouldBe Map( "int" -> "3", "str" -> "hello", "bool" -> "false" )

    }

}