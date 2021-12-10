package org.hungerford.generic.schema.primitives

import org.hungerford.generic.schema.{Primitive, PrimitiveSchemaBuilder}

object Primitives {

  given intSchema : Primitive[ Int ] = PrimitiveSchemaBuilder[ Int ]
    .description( s"Integer number between ${Int.MinValue} and ${Int.MaxValue}" )
    .build

  given doubleSchema : Primitive[ Double ] = PrimitiveSchemaBuilder[ Double ]
    .description( s"Floating point number between ${Double.MinValue} and ${Double.MaxValue} with maximum precision of ${Double.MinPositiveValue}" )
    .build

  given floatSchema : Primitive[ Float ] = PrimitiveSchemaBuilder[ Float ]
    .description( s"Floating point number between ${Float.MinValue} and ${Float.MaxValue} with maximum precision of ${Float.MinPositiveValue}" )
    .build

  given boolSchema : Primitive[ Boolean ] = PrimitiveSchemaBuilder[ Boolean ]
    .description( s"A type with two possible values: ${true} or ${false}" )
    .build

  given stringSchema : Primitive[ String ] = PrimitiveSchemaBuilder[ String ]
    .description( s"Text" )
    .build

}