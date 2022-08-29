package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.product.field.Field
import org.scalatest.flatspec.AnyFlatSpecLike

class AlignFilterTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

	behavior of "Align"

	it should "do nothing to empty tuples" in {
		type RV1 = EmptyTuple
		type R1 = EmptyTuple
		type RV2 = EmptyTuple
		type R2 = EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = EmptyTuple
		type ExpectedR = EmptyTuple
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "align single aligned fields" in {
		type RV1 = Int *: EmptyTuple
		type R1 = Field[String, Int, "field", Unit] *: EmptyTuple
		type RV2 = Int *: EmptyTuple
		type R2 = Field[Double, Int, "field", Unit] *: EmptyTuple

		summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]

	}

	it should "align two aligned fields" in {
		summon[ShapeMigration.Aux[Double, Field[String, Double, "field1", Unit], Nothing, Double, Field[Int, Double, "f1", Unit], Nothing]]
		summon[ShapeMigration.Aux[Int, Field[String, Int, "field2", Unit], Nothing, Int, Field[Int, Int, "f2", Unit], Nothing]]

		type RV1 = Double *: Int *: EmptyTuple
		type R1 = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: EmptyTuple
		type RV2 = Double *: Int *: EmptyTuple
		type R2 = Field[Int, Double, "f1", Unit] *: Field[Int, Int, "f2", Unit] *: EmptyTuple

		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = Double *: Int *: EmptyTuple
		type ExpectedR = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: EmptyTuple
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "align two unaligned fields" in {
		type RV1 = (Double, Int)
		type R1 = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: EmptyTuple
		type RV2 = (Int, Double)
		type R2 = Field[Int, Int, "f1", Unit] *: Field[Int, Double, "f2", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = (Int, Double)
		type ExpectedR = Field[String, Int, "field2", Unit] *: Field[String, Double, "field1", Unit] *: EmptyTuple
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "align five aligned fields" in {
		type RV1 = (Double, Int, Boolean, String, Char)
		type R1 = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: Field[String, Boolean, "field3", Unit] *: Field[String, String, "field4", Unit] *: Field[String, Char, "field5", Unit] *: EmptyTuple
		type RV2 = (Double, Int, Boolean, String, Char)
		type R2 = Field[String, Double, "f1", Unit] *: Field[String, Int, "f2", Unit] *: Field[String, Boolean, "f3", Unit] *: Field[String, String, "f4", Unit] *: Field[String, Char, "f5", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = RV1
		type ExpectedR = R1
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "align five unaligned fields" in {
		type RV1 = (Double, Int, Boolean, String, Char)
		type R1 = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: Field[String, Boolean, "field3", Unit] *: Field[String, String, "field4", Unit] *: Field[String, Char, "field5", Unit] *: EmptyTuple
		type RV2 = (Char, String, Boolean, Int, Double)
		type R2 = Field[String, Char, "f1", Unit] *: Field[String, String, "f2", Unit] *: Field[String, Boolean, "f3", Unit] *: Field[String, Int, "f4", Unit] *: Field[String, Double, "f5", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = RV2
		type ExpectedR = Field[String, Char, "field5", Unit] *: Field[String, String, "field4", Unit] *: Field[String, Boolean, "field3", Unit] *: Field[String, Int, "field2", Unit] *: Field[String, Double, "field1", Unit] *: EmptyTuple
		val res = align.alignValues((0.23, 5, false, "hello", 'G'))
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "realign two fields aligned by type if their field names can be aligned" in {
		type RV1 = (Double, Double)
		type R1 = Field[String, Double, "field2", Unit] *: Field[String, Double, "field1", Unit] *: EmptyTuple
		type RV2 = (Double, Double)
		type R2 = Field[Int, Double, "field1", Unit] *: Field[Int, Double, "field2", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = RV1
		type ExpectedR = Field[String, Double, "field1", Unit] *: Field[String, Double, "field2", Unit] *: EmptyTuple
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "realign fields aligned by type if their field names can be aligned, while leaving other aligned types aligned if their names cannot be aligned" in {
		type RV1 = (Double, Char, Double)
		type R1 = Field[String, Double, "field2", Unit] *: Field[String, Char, "a", Unit] *: Field[String, Double, "field1", Unit] *: EmptyTuple
		type RV2 = (Double, Char, Double)
		type R2 = Field[Int, Double, "field1", Unit] *: Field[String, Char, "b", Unit] *: Field[Int, Double, "field2", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = RV1
		type ExpectedR = Field[String, Double, "field1", Unit] *: Field[String, Char, "a", Unit] *: Field[String, Double, "field2", Unit] *: EmptyTuple
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

	it should "filter fields after alignment when the compared fields run out" in {
		type RV1 = (Double, Int, Boolean, String, Char)
		type R1 = Field[String, Double, "field1", Unit] *: Field[String, Int, "field2", Unit] *: Field[String, Boolean, "field3", Unit] *: Field[String, String, "field4", Unit] *: Field[String, Char, "field5", Unit] *: EmptyTuple
		type RV2 = (Double, Int, Boolean)
		type R2 = Field[String, Double, "f1", Unit] *: Field[String, Int, "f2", Unit] *: Field[String, Boolean, "f3", Unit] *: EmptyTuple
		val align = summon[AlignFilter[Forward, RV1, R1, Nothing, RV2, R2, Nothing]]
		type ExpectedRV = RV2
		type ExpectedR = (Field[String, Double, "field1", Unit], Field[String, Int, "field2", Unit], Field[String, Boolean, "field3", Unit])
		summon[align.RVOut =:= ExpectedRV]
		summon[align.ROut =:= ExpectedR]
	}

}
