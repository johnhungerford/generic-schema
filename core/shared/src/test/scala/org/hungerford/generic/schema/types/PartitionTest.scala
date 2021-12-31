package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class PartitionTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Partition"

    it should "partition an empty tuple into two empty tuples" in {
        val partitioner = summon[ Partition[ Numeric, EmptyTuple ] ]

        val (meets, fails) = partitioner.filter( EmptyTuple )
        meets shouldBe EmptyTuple
        fails shouldBe EmptyTuple
    }

    it should "partition a single succeeding tuple correctly" in {
        val partitioner = summon[ Partition[ Numeric, Tuple1[ Int ] ] ]

        val (meets, fails) = partitioner.filter( Tuple1( 5 ) )
        meets shouldBe Tuple1( 5 )
        fails shouldBe EmptyTuple
    }

    it should "partition a single failing tuple correctly" in {
        val partitioner = summon[ Partition[ Numeric, Tuple1[ String ] ] ]

        val (meets, fails) = partitioner.filter( Tuple1( "hello" ) )
        meets shouldBe EmptyTuple
        fails shouldBe Tuple1( "hello" )
    }

    it should "partition a complex tuple correctly" in {
        val partitioner = summon[ Partition[ Numeric, (Int, String, Double, Map[ String, String ], Long, Boolean) ] ]

        val (meets, fails) = partitioner.filter( (5, "hello", 0.2323D, Map( "hi" -> "there", "you" -> "two" ), 232432343L, false ) )
        meets shouldBe (5, 0.2323D, 232432343L)
        fails shouldBe ("hello", Map( "hi" -> "there", "you" -> "two" ), false)
    }
}
