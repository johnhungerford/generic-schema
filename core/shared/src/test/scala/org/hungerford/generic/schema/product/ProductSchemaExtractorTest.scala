package org.hungerford.generic.schema.product

import org.scalatest.flatspec.AnyFlatSpecLike

class ProductSchemaExtractorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
    behavior of "ProductSchemaExtractor"

    case class TC( int : Int, str : String )

    import generic.schema.exports.*

    it should "extract a schema from field descriptions" in {
        val tcSch = Schema.derived[ TC ]
        val tcProduct = tcSch.shape

        val strSch = ProductSchemaExtractor.extract[ String ]( tcProduct )

        strSch.shape shouldBe ()
    }

    case class TC1( flds : Map[ String, TC ] )

    it should "extract a schema from additional fields" in {
        val tcSch = Schema.derived[ TC ]

        val tc1Sch = Schema.productBuilder[ TC1 ]
          .additionalFields[ TC ].fromSchema( _.flds )( tcSch )
          .construct( af => TC1( af ) )
          .build

        val extractedTcSch = ProductSchemaExtractor.extract[ TC ]( tc1Sch.shape )

        extractedTcSch shouldBe tcSch
    }

    case class TCNest( tc : TC )
    case class TC2( nested : TCNest )

    it should "extract a schema from a nested field" in {
        val tcSch = Schema.derived[ TC ]
        import tcSch.given

        val tcNestSch = Schema.derived[ TCNest ] // should use tcSch for nested schema

        val tc2Sch = Schema.productBuilder[ TC2 ]
          .buildField[ TCNest ](
              _.extractor( _.nested )
                .name( "TCNest" )
                .fromSchema( tcNestSch )
                .build
          )
          .construct( tcn => TC2( tcn ) )
          .build

        val extractedTcSch = ProductSchemaExtractor.extract[ TC ]( tc2Sch.shape )

        extractedTcSch shouldBe tcSch
    }

    case class TC3( tc : TC, af : Map[ String, TC ] )

    it should "extract field schema prior to additional fields schema" in {
        val tcSch = Schema.derived[ TC ]
        val tcSch2 = tcSch.withDescription( "this one is for the tc field" )

        val tc3Sch = Schema.productBuilder[ TC3 ]
          .additionalFields[ TC ].fromSchema( _.af )( tcSch )
          .buildField[ TC ](
              _.extractor( _.tc )
                .name( "tc" )
                .fromSchema( tcSch2 )
                .build
          )
          .construct( (tc, af) => TC3( tc, af ) )
          .build

        val extractedTcSch = ProductSchemaExtractor.extract[ TC ]( tc3Sch.shape )

        extractedTcSch shouldBe tcSch2
        extractedTcSch should not be tcSch
    }

    case class TC4( nested : TCNest, tc : TC )

    it should "extract field schema prior to nested field schema" in {
        val tcSch = Schema.derived[ TC ]
        val tcSch2 = tcSch.withDescription( "this one is for the tc field" )

        val tcNestSch = {
            import tcSch.given
            Schema.derived[ TCNest ]
        }

        val tc4Sch = {
            import tcSch2.given
            Schema.derived[ TC4 ]
        }

        val extractedTcSch = ProductSchemaExtractor.extract[ TC ]( tc4Sch.shape )

        extractedTcSch shouldBe tcSch2
        extractedTcSch should not be tcSch
    }

    case class TC5( af : Map[ String, TC ], tc : TC )

    it should "extract additional field schema prior to nested field schema" in {
        val tcSch = Schema.derived[ TC ]
        val tcSch2 = tcSch.withDescription( "this one is for the tc field" )

        val tc5Sch = {
            import tcSch2.given
            Schema.derivedBuilder[ TC5 ]
              .removeField( "af" )
              .additionalFields[ TC ].fromSchema( _.af )( tcSch )
              .construct( (tc, af) => TC5( af, tc ) )
              .build
        }

        val extractedTcSch = ProductSchemaExtractor.extract[ TC ]( tc5Sch.shape )

        extractedTcSch shouldBe tcSch2
        extractedTcSch should not be tcSch
    }
}
