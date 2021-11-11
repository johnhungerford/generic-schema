package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers


class SchemaDeriverTest extends AnyFlatSpecLike with Matchers {

   behavior of "SchemaDeriver"

   it should "derive a product schema equivalent to one built with implicit primitives and no additional fields" in {
       case class Test( int : Int, str : String )

       import org.hungerford.generic.schema.Primitives._

       val testSchema = SchemaBuilder[ Test ]
         .product
         .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
         .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
         .construct( (tup, _) => Test(tup.head, tup.tail.head) )
         .deconstruct( value => ((value.int, value.str), Map.empty) )
         .build

       val newTestRes = SchemaDeriver.schema[ Test ]

       newTestRes.shape.fieldDescriptions shouldBe testSchema.shape.fieldDescriptions

   }

   it should "derive a product schema for a large case class" in {
       case class Test( int : Int, str : String, dbl : Double, bool : Boolean, newInt : Int )

       import Primitives._

       val newTestRes = SchemaDeriver.schema[ Test ]

       
   }

}
