package org.hungerford.generic.schema.coproduct

import org.scalatest.flatspec.AnyFlatSpecLike

class CoproductSchemaExtractorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
    import org.hungerford.generic.schema.Default.dsl.*

    behavior of "CoproductSchemaExtractor"

    sealed trait Sup
    case object TC extends Sup

    it should "extract a schema from a subtype" in {
        val tcSch = Schema.derived[ TC.type ]
        import tcSch.given
        val supSch = Schema.derived[ Sup ]

        val extractedTcSch = CoproductSchemaExtractor.extract[ TC.type ]( supSch.shape )

        extractedTcSch shouldBe tcSch
    }

    sealed trait Sup2
    sealed trait Sup2Nest extends Sup2
    case object TC2 extends Sup2Nest

    it should "extract a schema from a nested subtype" in {
        val tc2Sch = Schema.derived[ TC2.type ]
        import tc2Sch.given
        val sup2Sch = Schema.derived[ Sup2 ]

        val extractedTc2Sch = CoproductSchemaExtractor.extract[ TC2.type ]( sup2Sch.shape )

        extractedTc2Sch shouldBe tc2Sch
    }

    sealed trait Sup3
    sealed trait Sup3Nest extends Sup3
    case object TC3 extends Sup3 with Sup3Nest
    
    it should "extract a schema from a subtype prior to a nested subtype" in {
        val tc3Sch = Schema.derived[ TC3.type ]
        import tc3Sch.given

        val sup3Sch = Schema.derived[ Sup3 ]
          .modifyComponent( subtype( t[Sup3Nest] ) /~ t[TC3.type] )( _.modifySchema( _.withDescription( "twice nested" ) ) )

        val nestedTc3Sch = sup3Sch( subtype( t[Sup3Nest] ) /~ t[TC3.type] ).schema
        val topLevelTc3Sch = sup3Sch( subtype( t[TC3.type] ) ).schema

        topLevelTc3Sch shouldBe tc3Sch
        topLevelTc3Sch.genericDescription shouldBe None

        nestedTc3Sch should not be tc3Sch
        nestedTc3Sch.genericDescription should contain( "twice nested" )

        val extractedTc3Sch = CoproductSchemaExtractor.extract[ TC3.type ]( sup3Sch.shape )

        extractedTc3Sch shouldBe tc3Sch
        extractedTc3Sch should not be nestedTc3Sch
    }
}
