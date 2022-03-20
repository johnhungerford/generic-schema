package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype}
import org.hungerford.generic.schema.product.ProductShape
import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.{Default, Schema}
import org.hungerford.generic.schema.product.field.Field

class ValidDiscriminatorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ValidDiscriminator"

//    it should "exist for non-existent discriminators type/name" in {
//        assertCompiles( """summon[ ValidDiscriminator[ Unit, Nothing, Map[ Float, LazyList[ BigInt ] ] ] ]""" )
//    }
//
//    it should "exist for any discriminator type/name for an empty tuple" in {
//        assertCompiles( """summon[ ValidDiscriminator[ Int, Array[ String ], EmptyTuple ] ]""" )
//    }
//
//    case class P1( d : Int, other : Int )
//    val sch1 = Default.usingDsl { dsl =>
//        import dsl.{*, given}
//        Schema.derived[ P1 ]
//    }
//    val st1 = Subtype[ Any, P1, Int, "d", 1, "subtype-1", sch1.Shape ]( "subtype-1", sch1, v => v, { case v : P1 => Some( v ); case _ => None }, 1 )
//
//    it should "exist for a 1-tuple with a subtype of a product containing field type and name of discriminator" in {
//        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st1.Aux *: EmptyTuple ] ]""" )
//    }
//
//    case class P2( other : String, d : Int )
//    val sch2 = Default.usingDsl { dsl =>
//        import dsl.{*, given}
//        Schema.derived[ P2 ]
//    }
//    val st2 = Subtype[ Any, P2, Int, "d", 2, "subtype-2", sch2.Shape ]( "subtype-2", sch2, v => v, { case v : P2 => Some( v ); case _ => None }, 2 )
//
//    it should "exist for a 2-tuple with a two subtypes of products each containing field type and name of discriminator" in {
//        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st1.Aux *: st2.Aux *: EmptyTuple ] ]""" )
//        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st2.Aux *: st1.Aux *: EmptyTuple ] ]""" )
//    }
//
//    case class BadP2( other : String, e : Int )
//    val badSch2 = Default.usingDsl { dsl =>
//        import dsl.{*, given}
//        Schema.derived[ BadP2 ]
//    }
//    val badSt2 = Subtype[ Any, BadP2, Int, "e", 3, "subtype-2-bad",  badSch2.Shape ]( "subtype-2-bad", badSch2, v => v, { case v : BadP2 => Some( v ); case _ => None }, 3 )
//
//    it should "not exist for a 2-tuple with one subtype of a product lacking a field with the NAME of the discriminator" in {
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st1.Aux *: badSt2.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st2.Aux *: badSt2.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt2.Aux *: st1.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt2.Aux *: st2.Aux *: EmptyTuple ] ]""" )
//    }
//
//    case class BadP3( other : String, d : String )
//    val badSch3 = Default.usingDsl { dsl =>
//        import dsl.{*, given}
//        Schema.derived[ BadP3 ]
//    }
//    val badSt3 = Subtype[ Any, BadP3, String, "d", 4, "subtype-3-bad", badSch3.Shape ]( "subtype-3-bad", badSch3, v => v, { case v : BadP3 => Some( v ); case _ => None }, 4 )
//
//    it should "not exist for a 2-tuple with one subtype of a product lacking a field with TYPE of the discriminator" in {
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st1.Aux *: badSt3.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st2.Aux *: badSt3.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt3.Aux *: st1.Aux *: EmptyTuple ] ]""" )
//        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt3.Aux *: st2.Aux *: EmptyTuple ] ]""" )
//    }
}
