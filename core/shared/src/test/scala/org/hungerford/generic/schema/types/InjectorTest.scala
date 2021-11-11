//package org.hungerford.generic.schema.types
//
//import org.hungerford.generic.schema.types.Injector.Aux
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.scalatest.matchers.should.Matchers
//import shapeless._
//
//class InjectorTest extends AnyFlatSpecLike with Matchers {
//
//    behavior of "Injector"
//
//    it should "inject hlist elements into a target using only a simple element injector instance" in {
//        implicit def si[ T ] : Aux[ T, Map[String, String ], Int, Map[String, String ] ] = Injector.simpleInjector[ T, Map[ String, String ], Int ] {
//            ( from : T, into : Map[ String, String ], using : Int ) =>
//                into + (from.toString -> from.toString * using)
//        }
//
//        val tList : Int *: String *: Boolean *: EmptyTuple = 5 *: "hello" *: true *: EmptyTuple
//        val intList : Int *: Int *: Int *: EmptyTuple = 1 *: 2 *: 3 *: EmptyTuple
//
//        val flExtractor = implicitly[ Injector.Aux[ Int *: String *: Boolean *: EmptyTuple, Map[ String, String ], Int *: Int *: Int *: EmptyTuple, Map[ String, String ] ] ]
//
//        flExtractor.inject( tList, Map.empty, intList ) shouldBe Map( "5" -> "5", "hello" -> "hellohello", "true" -> "truetruetrue" )
//    }
//
//}
