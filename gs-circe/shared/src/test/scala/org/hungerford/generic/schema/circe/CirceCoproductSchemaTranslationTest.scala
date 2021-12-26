package org.hungerford.generic.schema.circe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.{Schema, Default}
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.syntax.{given, *}
import io.circe.*

class CirceCoproductSchemaTranslationTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Encoding"

    sealed trait SuperT
    object SuperT {
        case class SubT1( int : Int, str : String ) extends SuperT
        case class SubT2( dbl : Double, flt : Float, bool : Boolean ) extends SuperT
    }

    it should "encode a json value" in {
        import Default.dsl.*
        import CirceSchemaTranslation.given

        val subt1Sch = Schema.derived[ SuperT.SubT1 ]
        import subt1Sch.givenSchema

        val subt2Sch = Schema.derived[ SuperT.SubT2 ]
        import subt2Sch.givenSchema

        val superTSch = Schema.derived[ SuperT ]

        given Codec[ SuperT ] = SchemaTranslator.translate( superTSch )

        val value : SuperT = SuperT.SubT2( 0.234D, 342.231F, false )
        value.asJson.noSpaces.toString shouldBe """{"dbl":0.234,"flt":342.231,"bool":false}"""
    }

}
