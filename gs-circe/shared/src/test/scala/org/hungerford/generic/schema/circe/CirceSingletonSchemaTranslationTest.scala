package org.hungerford.generic.schema.circe

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.scalatest.flatspec.AnyFlatSpecLike

import io.circe.{Codec, Json}

class CirceSingletonSchemaTranslationTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "CirceSingletonSchemaTranslation"

    case object TestSingleton

    it should "translate a singleton to a value that can be encoded and decoded" in {
        import org.hungerford.generic.schema.Default.dsl.*
        val sch = Schema.derived[ TestSingleton.type ]

        import org.hungerford.generic.schema.circe.CirceSchemaTranslation.given

        given codec : Codec[ TestSingleton.type ] = SchemaTranslator.translate( sch )

        codec( TestSingleton ).toString shouldBe """"TestSingleton""""

        codec( Json.fromString( "TestSingleton" ).hcursor ) match {
            case Left( decodeFail ) => fail( decodeFail.message )
            case Right( v ) => v shouldBe TestSingleton
        }
    }

    sealed trait SuperT
    case object SubT extends SuperT

    it should "translate a singleton within a coproduct" in {
        import org.hungerford.generic.schema.Default.dsl.*
        val sch = Schema.derived[ SuperT ]

        import org.hungerford.generic.schema.circe.CirceSchemaTranslation.given

        given codec : Codec[ SuperT ] = SchemaTranslator.translate( sch )

        val testVal : SuperT = SubT

        codec( testVal ).toString shouldBe """"SubT""""

        codec( Json.fromString( "SubT" ).hcursor ) match {
            case Left( decodeFail ) => fail( decodeFail.message )
            case Right( v ) => v shouldBe SubT
        }
    }


}
