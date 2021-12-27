package org.hungerford.generic.schema.circe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.{Default, Schema}
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.syntax.{*, given}
import io.circe.*
import org.hungerford.generic.schema.validator.Validator
import scala.util.Try

class CirceCoproductSchemaTranslationTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Encoding"

    sealed trait SuperT
    object SuperT {
        case class SubT1( int : Int, str : String ) extends SuperT
        case class SubT2( dbl : Double, flt : Float, bool : Boolean ) extends SuperT
    }

    it should "encode a sealed trait correctly to json" in {
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

    case class Super( str : String )
    case class Sub( str : String )

    it should "encode a non-sealed trait coproduct (a custom coproduct) to json correctly" in {
        import Default.dsl.*
        import CirceSchemaTranslation.given

        val sch = Schema.coproductBuilder[ Super ]
          .buildSubtype[ Int ](
              _.typeName( "int" )
                .validate( Validator.negativeOrZero, Validator.nonZero )
                .primitive
                .toSuper( v => Super( v.toString ) )
                .fromSuper( v => Try( { val res = v.str.toInt; if res > 0 then res else throw Exception() } ).toOption )
                .build
          )
          .buildSubtype[ Double ](
              _.typeName( "double" )
                .validate( Validator.positiveOrZero )
                .primitive
                .toSuper( v => Super( v.toString ) )
                .fromSuper( v => Try( { val res = v.str.toDouble; if res <= 0 then res else throw Exception() } ).toOption )
                .build
          )
          .buildSubtype[ Sub ](
              _.typeName( "Sub" )
                .fromSchema( Schema.derived[ Sub ] )
                .toSuper( v => Super( v.str ) )
                .fromSuper( v => Some( Sub( v.str ) ) )
                .build
          )
          .build

        given Codec[ Super ] = SchemaTranslator.translate( sch )

        val val1 : Super = Super( "-32.4353" )
        val1.asJson.noSpaces.toString shouldBe """-32.4353"""

        val val2 : Super = Super( "3454" )
        val2.asJson.noSpaces.toString shouldBe """3454"""

        val val3 : Super = Super( "hello world" )
        val3.asJson.noSpaces.toString shouldBe """{"str":"hello world"}"""
    }

}
