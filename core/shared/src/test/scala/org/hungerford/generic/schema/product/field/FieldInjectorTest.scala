package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.validator.Validator

class FieldInjectorTest extends AnyFlatSpecLike with Matchers {

    case class ID[ T ]()

    behavior of "FieldInjector"

    it should "inject fields into an object" in {

        val intFd = FieldBuilder[ String, Int ].extractor( _.toInt ).name( "int" ).primitive.build
        val strFd = FieldBuilder[ String, String ].extractor( v => v ).name( "str" ).primitive.build
        val boolFd = FieldBuilder[ String, Boolean ].extractor( _.toBoolean ).name( "bool" ).primitive.build

        val fds = intFd *: strFd *: boolFd *: EmptyTuple

        given [ T, F, N <: FieldName, S ] : FieldInjector[ T, F, N, S, Map[ String, String ] ] = {
            new FieldInjector[ T, F, N, S, Map[ String, String ] ] {
                def inject( field : Field[ T, F, N, S ], value : F, into : Map[ String, String ] ) : Map[ String, String ] = {
                    into + (field.fieldName -> value.toString)
                }
            }
        }

        val injectionResults = FieldInjector.inject( fds, 3 *: "hello" *: false *: EmptyTuple, Map.empty[ String, String ] )

        injectionResults shouldBe Map( "int" -> "3", "str" -> "hello", "bool" -> "false" )

    }

}