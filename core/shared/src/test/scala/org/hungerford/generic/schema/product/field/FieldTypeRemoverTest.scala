package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.SchemaDeriver
import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.types.Nat

class FieldTypeRemoverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "FieldTypeRemover"

    it should "not exist for an empty tuple" in {
        assertDoesNotCompile( """summon[FieldTypeRemover[ Int, Nat._0, EmptyTuple ] ]""" )
    }

    it should "exist for a N=0 remover for a single field tuple or a single lazy field tuple of the same type" in {
        summon[FieldTypeRemover.Aux[Int, Nat._0, Field[ Double, Int, "int", Unit] *: EmptyTuple, EmptyTuple]]
        summon[FieldTypeRemover.Aux[Int, Nat._0, LazyField[ Double, Int, "int"] *: EmptyTuple, EmptyTuple]]
    }

    it should "not exist for a N>0 remover for a single field tuple or a single lazy field tuple of the same type" in {
        assertDoesNotCompile( """summon[FieldTypeRemover[Int, Nat._1, Field[ Double, Int, "int", Unit] *: EmptyTuple ]]""" )
        assertDoesNotCompile( """summon[FieldTypeRemover[Int, Nat._1, LazyField[ Double, Int, "int"] *: EmptyTuple ]]""" )
        assertDoesNotCompile( """summon[FieldTypeRemover[Int, Nat._20, Field[ Double, Int, "int", Unit] *: EmptyTuple ]]""" )
        assertDoesNotCompile( """summon[FieldTypeRemover[Int, Nat._20, LazyField[ Double, Int, "int"] *: EmptyTuple ]]""" )
    }

    it should "exist for a N=0 remover for a field in a tuple>1 of fields and lazy fields" in {
        summon[FieldTypeRemover.Aux[Int, Nat._0, Field[ Double, String, "str", Unit] *: Field[ Double, Int, "int", Unit] *: EmptyTuple, Field[ Double, String, "str", Unit] *: EmptyTuple]]
        summon[FieldTypeRemover.Aux[Int, Nat._0, LazyField[ Double, String, "str"] *: LazyField[ Double, Int, "int"] *: EmptyTuple, LazyField[ Double, String, "str"] *: EmptyTuple]]
        summon[FieldTypeRemover.Aux[Int, Nat._0, LazyField[ Double, String, "str"] *: Field[ Double, Int, "int", Unit] *: EmptyTuple, LazyField[ Double, String, "str" ] *: EmptyTuple]]
        summon[FieldTypeRemover.Aux[Int, Nat._0, Field[ Double, String, "str", Unit] *: LazyField[ Double, Int, "int"] *: EmptyTuple, Field[ Double, String, "str", Unit] *: EmptyTuple]]
    }

    case class Test1( int : Int, str : String, bool : Boolean, flt : Float, str2 : String, dbl : Double, str3 : String )
    val test1Shape = SchemaDeriver.schema[ Test1 ].shape
    val test1R = test1Shape.fieldDescriptions

    it should "be able to remove a type from an actual R" in {
        val remover = summon[ FieldTypeRemover[ Float, Nat._0, test1Shape.R ] ]
        val removed = remover.remove( test1R )
        removed.size shouldBe 6
        removed.tail.tail.tail.head.fieldName shouldBe "str2"
    }

    it should "be able to remove a first type from an actual R using N=0" in {
        val remover = summon[ FieldTypeRemover[ String, Nat._0, test1Shape.R ] ]
        val removed = remover.remove( test1R )
        removed.size shouldBe 6
        removed.tail.head.fieldName shouldBe "bool"
    }

    it should "be able to remove a second type from an actual R using N=1" in {
        val remover = summon[ FieldTypeRemover[ String, Nat._1, test1Shape.R ] ]
        val removed = remover.remove( test1R )
        removed.size shouldBe 6
        removed.tail.head.fieldName shouldBe "str"
        removed.tail.tail.tail.tail.head.fieldName shouldBe "dbl"
    }

    it should "be able to remove a third type from an actual R using N=2" in {
        val remover = summon[ FieldTypeRemover[ String, Nat._2, test1Shape.R ] ]
        val removed = remover.remove( test1R )
        removed.size shouldBe 6
        removed.tail.head.fieldName shouldBe "str"
        removed.tail.tail.tail.tail.head.fieldName shouldBe "str2"
        removed.tail.tail.tail.tail.tail.head.fieldName shouldBe "dbl"
        removed.tail.tail.tail.tail.tail.tail shouldBe EmptyTuple
    }

}
