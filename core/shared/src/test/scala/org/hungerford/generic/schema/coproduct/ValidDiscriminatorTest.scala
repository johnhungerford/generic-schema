package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeCase}
import org.hungerford.generic.schema.product.ProductShape
import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.{Default, Schema}
import org.hungerford.generic.schema.product.field.Field

class ValidDiscriminatorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ValidDiscriminator"

    it should "exist for non-existent discriminators type/name" in {
        assertCompiles( """summon[ ValidDiscriminator[ Nothing, Nothing, Map[ Float, LazyList[ BigInt ] ] ] ]""" )
    }

    it should "exist for any discriminator type/name for an empty tuple" in {
        assertCompiles( """summon[ ValidDiscriminator[ Int, Array[ String ], EmptyTuple ] ]""" )
    }

    case class P1( d : Int, other : Int )
    val sch1 = Default.usingDsl { dsl =>
        import dsl.{*, given}
        Schema.derived[ P1 ]
    }
    val st1 = SubtypeCase( "subtype-1", sch1 )

    it should "exist for a 1-tuple with a subtype of a product containing field type and name of discriminator" in {
        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st1.ST *: EmptyTuple ] ]""" )
    }

    case class P2( other : String, d : Int )
    val sch2 = Default.usingDsl { dsl =>
        import dsl.{*, given}
        Schema.derived[ P2 ]
    }
    val st2 = SubtypeCase( "subtype-2", sch2 )

    it should "exist for a 2-tuple with a two subtypes of products each containing field type and name of discriminator" in {
        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st1.ST *: st2.ST *: EmptyTuple ] ]""" )
        assertCompiles( """summon[ ValidDiscriminator[ Int, "d", st2.ST *: st1.ST *: EmptyTuple ] ]""" )
    }

    case class BadP2( other : String, e : Int )
    val badSch2 = Default.usingDsl { dsl =>
        import dsl.{*, given}
        Schema.derived[ BadP2 ]
    }
    val badSt2 = SubtypeCase( "subtype-2-bad", badSch2 )

    it should "not exist for a 2-tuple with one subtype of a product lacking a field with the NAME of the discriminator" in {
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st1.ST *: badSt2.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st2.ST *: badSt2.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt2.ST *: st1.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt2.ST *: st2.ST *: EmptyTuple ] ]""" )
    }

    case class BadP3( other : String, d : String )
    val badSch3 = Default.usingDsl { dsl =>
        import dsl.{*, given}
        Schema.derived[ BadP3 ]
    }
    val badSt3 = SubtypeCase( "subtype-3-bad", badSch3 )

    it should "not exist for a 2-tuple with one subtype of a product lacking a field with TYPE of the discriminator" in {
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st1.ST *: badSt3.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", st2.ST *: badSt3.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt3.ST *: st1.ST *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminator[ Int, "d", badSt3.ST *: st2.ST *: EmptyTuple ] ]""" )
    }
}
