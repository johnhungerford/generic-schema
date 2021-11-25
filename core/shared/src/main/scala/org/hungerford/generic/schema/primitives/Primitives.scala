package org.hungerford.generic.schema

import org.hungerford.generic.schema.SchemaBuilder

object Primitives {

  given intSchema : Primitive[ Int ] = SchemaBuilder[ Int ]
    .primitive
    .description( s"Integer number between ${Int.MinValue} and ${Int.MaxValue}" )
    .build

  given doubleSchema : Primitive[ Double ] = SchemaBuilder[ Double ]
    .primitive
    .description( s"Floating point number between ${Double.MinValue} and ${Double.MaxValue} with maximum precision of ${Double.MinPositiveValue}" )
    .build

  given floatSchema : Primitive[ Float ] = SchemaBuilder[ Float ]
    .primitive
    .description( s"Floating point number between ${Float.MinValue} and ${Float.MaxValue} with maximum precision of ${Float.MinPositiveValue}" )
    .build

  given boolSchema : Primitive[ Boolean ] = SchemaBuilder[ Boolean ]
    .primitive
    .description( s"A type with two possible values: ${true} or ${false}" )
    .build

  given stringSchema : Primitive[ String ] = SchemaBuilder[ String ]
    .primitive
    .description( s"Text" )
    .build

}