package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._


class SchemaDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaDeriver"

    it should "derive a product schema equivalent to one built with implicit primitives and no additional fields" in {
        case class Test( int : Int, str : String )

        import primitives._

        val testSchema = SchemaBuilder[ Test ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].fromSchema.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str" ).build )
          .construct( (tup, _) => Test( tup._1, tup._2) )
          .deconstruct( value => ((value.int, value.str), Map.empty) )
          .build

        val newTestRes = SchemaDeriver.schema[ Test ]

        newTestRes.shape.fieldDescriptions shouldBe testSchema.shape.fieldDescriptions

    }

}
