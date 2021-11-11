//package org.hungerford.generic.schema.types
//
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.scalatest.matchers.should.Matchers
//import shapeless._
//import shapeless.ops.hlist.Prepend
//
//class ExtractorTest extends AnyFlatSpecLike with Matchers {
//
//    behavior of "field extractor"
//
//    it should "extract to a list" in {
//
//        implicit def testSimpleExtractor : SimpleExtractor.Aux[ Map[ Int, String ], Int, String ] = {
//            new SimpleExtractor[ Map[ Int, String ], Int ] {
//                override type Out = String
//
//                override def extract( from : Map[ Int, String ], using : Int ) : Out = {
//                    from( using )
//                }
//            }
//        }
//
//        implicitly[ Extractor[ Map[ Int, String ], EmptyTuple, Int ] ].extract( Map( 1 -> "one" ), EmptyTuple, 1 ) shouldBe "one" *: EmptyTuple
//
//        val res = implicitly[ Extractor.Aux[ Map[ Int, String ], EmptyTuple, Int *: Int *: Int *: EmptyTuple, String *: String *: String *: EmptyTuple ] ]
//          .extract( Map( 1 -> "one", 2 -> "two", 3 -> "three" ), EmptyTuple, 1 *: 2 *: 3 *: EmptyTuple )
//
//        res shouldBe "one" *: "two" *: "three" *: EmptyTuple
//    }
//
//}
