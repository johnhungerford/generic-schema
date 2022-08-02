package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import org.scalatest.flatspec.AnyFlatSpecLike

object SingletonJsonTranslationTest {
    case object TestSingleton

    sealed trait SuperT

    case object SubT extends SuperT
}

abstract class SingletonJsonTranslationTest[ OtherSchema[ _ ] ]
  extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    import SingletonJsonTranslationTest.*
    import generic.schema.exports.*

    val tsSchema = Schema.derived[ TestSingleton.type ]
    val stSchema = Schema.derived[ SuperT ]

    def tsOSchema: OtherSchema[ TestSingleton.type ]

    def stOSchema: OtherSchema[ SuperT ]

    def writeJson[ T ]( value: T, schm: OtherSchema[ T ] ): String

    def readJson[ T ]( json: String, schm: OtherSchema[ T ] ): Option[ T ]

    behavior of "SingletonSchemaTranslation"

    it should "translate a singleton to a value that can be encoded and decoded" in {
        writeJson( TestSingleton, tsOSchema ) shouldBe """"TestSingleton""""

        readJson( "\"TestSingleton\"", tsOSchema ) shouldBe Some( TestSingleton )
    }

//    it should "translate a singleton within a coproduct" in {
//        val testVal: SuperT = SubT
//
//        writeJson( testVal, stOSchema ) shouldBe """"SubT""""
//
//        readJson[ SuperT ]( "\"SubT\"", stOSchema ) shouldBe Some( SubT )
//    }

}
