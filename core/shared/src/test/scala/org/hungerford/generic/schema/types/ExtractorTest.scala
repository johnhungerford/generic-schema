package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import shapeless.ops.hlist.Prepend

class ExtractorTest extends AnyFlatSpecLike with Matchers {

    behavior of "field extractor"

    it should "extract to a list" in {

//        implicit def testExtractor[ L <: HList, Res <: HList ](
//            implicit
//            prep : Prepend.Aux[ L, String :: HNil, Res ],
//        ) : Extractor.Aux[ Map[ Int, String ], L, Int, prep.Out ] =
//            new Extractor[ Map[ Int, String ], L, Int ] {
//                override type Out = Res
//
//                override def extract( from : Map[ Int, String ], to : L, using : Int ) : Res =
//                    to :+ from( using )
//            }

        implicit def testSimpleExtractor : SimpleExtractor.Aux[ Map[ Int, String ], Int, String ] = {
            new SimpleExtractor[ Map[ Int, String ], Int ] {
                override type Out = String

                override def extract( from : Map[ Int, String ], using : Int ) : Out = {
                    from( using )
                }
            }
        }

        implicitly[ Extractor[ Map[ Int, String ], HNil, Int ] ].extract( Map( 1 -> "one" ), HNil, 1 ) shouldBe "one" :: HNil

        val res = implicitly[ Extractor.Aux[ Map[ Int, String ], HNil, Int :: Int :: Int :: HNil, String :: String :: String :: HNil ] ]
          .extract( Map( 1 -> "one", 2 -> "two", 3 -> "three" ), HNil, 1 :: 2 :: 3 :: HNil )

        res shouldBe "one" :: "two" :: "three" :: HNil
    }

}
