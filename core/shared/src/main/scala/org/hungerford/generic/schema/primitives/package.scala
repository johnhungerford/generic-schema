package org.hungerford.generic.schema

package object primitives {

    implicit val intSchema : Primitive[ Int ] =
        SchemaBuilder[ Int ]
          .primitive
          .description( s"Integer number between ${Int.MinValue} and ${Int.MaxValue}" )
          .build

    implicit val doubleSchema : Primitive[ Double ] =
        SchemaBuilder[ Double ]
          .primitive
          .description( s"Floating point number between ${Double.MinValue} and ${Double.MaxValue} with maximum precision of ${Double.MinPositiveValue}" )
          .build

    implicit val floatSchema : Primitive[ Float ] =
        SchemaBuilder[ Float ]
          .primitive
          .description( s"Floating point number between ${Float.MinValue} and ${Float.MaxValue} with maximum precision of ${Float.MinPositiveValue}" )
          .build

    implicit val boolSchema : Primitive[ Boolean ] =
        SchemaBuilder[ Boolean ]
          .primitive
          .description( s"A type with two possible values: ${true} or ${false}" )
          .build

    implicit val stringSchema : Primitive[ String ] =
        SchemaBuilder[ String ]
          .primitive
          .description( s"Text" )
          .build

}
