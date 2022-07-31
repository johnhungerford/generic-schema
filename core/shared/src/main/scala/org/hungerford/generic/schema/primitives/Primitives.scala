package org.hungerford.generic.schema.primitives

import org.hungerford.generic.schema
import org.hungerford.generic.schema.coproduct.CoproductSchemaBuilder
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.types.TypeName
import org.hungerford.generic.schema.{Primitive, PrimitiveSchemaBuilder, Schema, SchemaProvider}

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

  transparent inline given optionSchema[ T, TS ](
      using
      inner: Schema.Aux[ T, TS ],
      tn: TypeName[ T ],
  ) : Schema[ Option[ T ] ] = {
      val sch = CoproductSchemaBuilder.empty[ Option[ T ] ]
        .description( s"Optional value of ${ inner.name.getOrElse( "(unnamed inner type)" ) }" )
        .buildSubtype[ None.type ](
            _.typeName( "Empty" ).fromSuper( { case None => Some( None ); case _ => None } ).singleton.build
        )
        .buildSubtype[ T ](
            _.typeName[ tn.Name ]( tn.name )
              .fromSuper( { case Some( t ) => Some( t ); case _ => None } )
              .toSuper( t => Some( t ) )
              .fromSchema[ TS ]
              .build
        )
        .build

      sch
  }
}
