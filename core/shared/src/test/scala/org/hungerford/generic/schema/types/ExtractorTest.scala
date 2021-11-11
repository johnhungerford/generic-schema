// package org.hungerford.generic.schema.types

// import org.scalatest.flatspec.AnyFlatSpecLike
// import org.scalatest.matchers.should.Matchers

// class ExtractorTest extends AnyFlatSpecLike with Matchers {

//    behavior of "field extractor"

//    it should "extract to a tuple" in {

//        given testSimpleExtractor : SimpleExtractor.Aux[ Map[ Int, String ], Int, String ] = {
//            new SimpleExtractor[ Map[ Int, String ], Int ] {
//                override type Out = String

//                override def extract( from : Map[ Int, String ], informedBy : Int ) : Out = {
//                    from( informedBy )
//                }
//            }
//        }

//        val res = Extractor.extractor[ Map[ Int, String ], EmptyTuple, Int *: EmptyTuple, String *: EmptyTuple ]
//     //    val res = summon[ Extractor[ Map[ Int, String ], EmptyTuple, Int *: EmptyTuple ] ]
//     //      .extract( Map( 1 -> "one", 2 -> "two", 3 -> "three" ), EmptyTuple, 1 *: EmptyTuple )

//        res shouldBe "one" *: EmptyTuple
//    }

// }
