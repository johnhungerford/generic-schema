package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.validator.Validator

class FieldExtractorTest extends AnyFlatSpecLike with Matchers {

    case class ID[ T ]()

    behavior of "FieldExtractor"

    given [ X ] : FieldExtractor[ Map[ String, String ], X, ID ] with {
        type Out = String

        def extract( from : Map[ String, String ], informedBy : TranslatedFieldDescription[ X, ID ] ) : String = {
            from( informedBy.fieldName )
        }
    }

    it should "extract from field descriptions" in {
        val intTfd = TranslatedFieldDescription[ Int, ID ]( "int", new ID[ Int ](), None, Set.empty[ Validator[ Int ] ] )
        val strTfd = TranslatedFieldDescription[ String, ID ]( "str", new ID[ String ](), None, Set.empty[ Validator[ String ] ] )
        val boolTfd = TranslatedFieldDescription[ Boolean, ID ]( "bool", new ID[ Boolean ](), None, Set.empty[ Validator[ Boolean ] ] )
        val dblTfd = TranslatedFieldDescription[ Double, ID ]( "dbl", new ID[ Double ](), None, Set.empty[ Validator[ Double ] ] )

        val fds = intTfd *: strTfd *: boolTfd *: dblTfd *: EmptyTuple

        import org.hungerford.generic.schema.types.SimpleExtractor

        val source = Map( "dbl" -> "DOUBLE FIELD", "int" -> "INTEGER FIELD", "str" -> "STRING FIELD", "bool" -> "BOOLEAN FIELD" )

        FieldsExtractor.extract( source, fds ) shouldBe "INTEGER FIELD" *: "STRING FIELD" *: "BOOLEAN FIELD" *: "DOUBLE FIELD" *: EmptyTuple
    }


}
