package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Default, Schema}
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.util.Try

sealed trait SuperT
case class SubT1( int : Int, str : String ) extends SuperT
case class SubT2( dbl : Double, lng : Long, bool : Boolean ) extends SuperT
case object SubT3 extends SuperT

case class Super( str : String )
case class Sub( subStr : String )

sealed trait RecurT
case object Terminal extends RecurT
case class Rec(a: RecurT, b: Int) extends RecurT

abstract class CoproductJsonTranslationTest[ OtherSchema[ _ ] ](
    using
    intSch: OtherSchema[ Int ],
    strSch: OtherSchema[ String ],
    dblSch: OtherSchema[ Double ],
    boolSch: OtherSchema[ Boolean ],
) extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    import Default.dsl.*

    val subt1Sch = Schema.derived[ SubT1 ]
    import subt1Sch.givenSchema

    val subt2Sch = Schema.derived[ SubT2 ]
    import subt2Sch.givenSchema

    val superTSch = Schema.derived[ SuperT ]

    val superSch = Schema.coproductBuilder[ Super ]
      .buildSubtype[ Sub ](
          _.typeName( "Sub" )
            .validate( Validator( v => Try( v.subStr.toInt >= 0 ).getOrElse( true ) && Try( v.subStr.toDouble < 0 ).getOrElse( true ) ) )
            .fromSchema( Schema.derived[ Sub ] )
            .toSuper( v => Super( v.subStr ) )
            .fromSuper( v => Some( Sub( v.str ) ) )
            .build
          )
      .buildSubtype[ Int ](
          _.typeName( "int" )
            .validate( Validator.negativeOrZero, Validator.nonZero )
            .primitive
            .toSuper( v => Super( v.toString ) )
            .fromSuper( v => Try( v.str.toInt ).toOption )
            .build
          )
      .buildSubtype[ Double ](
          _.typeName( "double" )
            .validate( Validator.positiveOrZero )
            .primitive
            .toSuper( v => Super( v.toString ) )
            .fromSuper( v => Try( v.str.toDouble ).toOption )
            .build
          )
      .build

    val recurSch = Schema.derived[ Rec ]

    def superTOs : OtherSchema[ SuperT ]
    def superOs : OtherSchema[ Super ]
    def recurSchOs : OtherSchema[ Rec ]

    def writeJson[ T ]( value: T, schm: OtherSchema[ T ] ): String
    def readJson[ T ]( value: String, schm: OtherSchema[ T ] ): T

    behavior of "CoproductSchemaTranslator Encoding"

    it should "encode a sealed trait correctly to json" in {
        val value : SuperT = SubT2( 0.234D, 3422315, false )
        writeJson( value, superTOs ) shouldBe """{"dbl":0.234,"lng":3422315,"bool":false}"""

        val singleVal : SuperT = SubT3
        writeJson( singleVal, superTOs ) shouldBe """"SubT3""""
    }

    it should "decode a sealed trait correctly from json" in {
        readJson[ SuperT ]( """{"dbl":0.234,"lng":3422315,"bool":false}""", superTOs ) shouldBe SubT2( 0.234D, 3422315, false )

        readJson[ SuperT ]( """"SubT3"""", superTOs ) shouldBe SubT3
    }

    it should "encode a non-sealed-trait coproduct (a custom coproduct) to json correctly, using validators to choose cases" in {
        val val1 : Super = Super( "32.4353" )
        writeJson( val1, superOs ) shouldBe """32.4353"""

        val val2 : Super = Super( "-3454" )
        writeJson( val2, superOs ) shouldBe """-3454"""

        val val4 : Super = Super( "-23.343" )
        writeJson( val4, superOs ) shouldBe """{"subStr":"-23.343"}"""
    }

    it should "decode a non-sealed-trait coproduct (a custom coproduct) from json correctly, using validators to choose cases" in {
        val val1 : Super = Super( "32.4353" )
        readJson[ Super ]( """"32.4353"""", superOs ) shouldBe val1

        val val2 : Super = Super( "-3454" )
        readJson[ Super ]( """"-3454"""", superOs ) shouldBe val2

        val val4 : Super = Super( "-23.343" )
        readJson[ Super ]( """{"subStr":"-23.343"}""", superOs ) shouldBe val4
    }

    it should "encode a product with a recursive coproduct field" in {
        val val1 : Rec = Rec( Rec( Rec( Rec( Terminal, 2 ), 3 ), 4 ), 5 )
        writeJson( val1, recurSchOs ) shouldBe """{"a":{"a":{"a":{"a":"Terminal","b":2},"b":3},"b":4},"b":5}"""
    }

    it should "decode a product with a recursive coproduct field" in {
        val val1 : String = """{"a":{"a":{"a":{"a":"Terminal","b":2},"b":3},"b":4},"b":5}"""
        readJson[ Rec ]( val1, recurSchOs ) shouldBe Rec( Rec( Rec( Rec( Terminal, 2 ), 3 ), 4 ), 5 )
    }

}
