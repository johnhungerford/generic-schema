package org.hungerford.generic.schema.defaults

import org.hungerford.generic.schema
import org.hungerford.generic.schema.coproduct.CoproductSchemaBuilder
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.types.TypeName
import org.hungerford.generic.schema.{Primitive, PrimitiveSchemaBuilder, Schema, SchemaProvider}

import scala.compiletime.summonInline
import scala.reflect.ClassTag

trait DefaultSchemas {

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

  transparent inline given someSchema[ T : ClassTag, TS ](
      using
      inner: Schema.Aux[ T, TS ],
  ): Schema[ Some[ T ] ] = {
      val ct = summon[ClassTag[T]]
      val tName = ct.runtimeClass.getName.split("""\.""").lastOption.getOrElse( "T" )
        .split("""\$""").lastOption.getOrElse( "T" )
      ProductSchemaBuilder[ Some[ T ] ]
        .name(s"Some[$tName]")
        .description( s"Presence of optional value of type ${ inner.name.getOrElse( tName ) }" )
        .buildField[ T ](
            _.name( "value" )
              .extractor( _.value )
              .fromSchema( inner )
              .build
         )
        .construct( value => Some( value ) )
        .build
  }

  transparent inline given optionSchema[ T : ClassTag, TS ](
      using
      inner: Schema.Aux[ T, TS ],
  ) : Schema[ Option[ T ] ] = {
      val ct = summon[ClassTag[T]]
      val tName = ct.runtimeClass.getName.split("""\.""").lastOption.getOrElse( "T" )
        .split("""\$""").lastOption.getOrElse( "T" )
      inline summonInline[Schema[Some[T]]] match {
          case someSch : Schema.Aux[Some[T], sts] =>
              type STS = sts
              CoproductSchemaBuilder.empty[ Option[ T ] ]
                .name(s"Option[$tName]")
                .description( s"Optional value of ${ inner.name.getOrElse( tName ) }" )
                .buildSubtype[ None.type ](
                    _.typeName( "Empty" ).fromSuper( { case None => Some( None ); case _ => None } ).singleton.build
                 )
                .buildSubtype[ Some[ T ] ](
                    _.typeName( "NonEmpty" )
                      .fromSuper( { case Some( t ) => Some( Some( t ) ); case _ => None } )
                      .toSuper( t => t )
                      .fromSchema[STS](using someSch)
                      .build
                    )
                .build
      }
  }
}

object DefaultSchemas extends DefaultSchemas
