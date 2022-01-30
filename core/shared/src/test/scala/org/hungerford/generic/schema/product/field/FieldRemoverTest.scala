package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*

class FieldRemoverTest extends AnyFlatSpecLike with Matchers {

     behavior of "FieldRemover"

     case class TestClass( int : Int, str : String, bool : Boolean )

     val fields = Schema.derivedBuilder[ TestClass ]
       .build
       .shape
       .fieldDescriptions

     it should "remove a field based on a name" in {
          val res = FieldRemover.remove( "int", fields )

          res.size shouldBe 2
          res.head.fieldName shouldBe "str"
          res.tail.head.fieldName shouldBe "bool"

          val res1 = FieldRemover.remove( "str", fields )

          res1.size shouldBe 2
          res1.head.fieldName shouldBe "int"
          res1.tail.head.fieldName shouldBe "bool"

          val res2 = FieldRemover.remove( "bool", fields )

          res2.size shouldBe 2
          res2.head.fieldName shouldBe "int"
          res2.tail.head.fieldName shouldBe "str"
     }

     it should "remove a field based on index" in {
          val res = FieldRemover.remove( 0, fields )

          res.size shouldBe 2
          res.head.fieldName shouldBe "str"
          res.tail.head.fieldName shouldBe "bool"

          val res1 = FieldRemover.remove( 1, fields )

          res1.size shouldBe 2
          res1.head.fieldName shouldBe "int"
          res1.tail.head.fieldName shouldBe "bool"

          val res2 = FieldRemover.remove( 2, fields )

          res2.size shouldBe 2
          res2.head.fieldName shouldBe "int"
          res2.tail.head.fieldName shouldBe "str"
     }

}
