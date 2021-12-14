package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.{ProductDeriver, ProductSchemaBuilder, ProductShape, TupleIntLength}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.validator.Validator

case class PrimitiveSchemaBuilder[ T ](
   private[ schema ] val nm : Option[ String ] = None,
   private[ schema ] val desc : Option[ String ] = None,
   private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ schema ] val exs : Seq[ T ] = Nil,
   private[ schema ] val dep : Boolean = false,
) {

   def name( name : String ) : PrimitiveSchemaBuilder[ T ] = copy( nm = Some( name ) )

   def description( description : String ) : PrimitiveSchemaBuilder[ T ] = copy( desc = Some( description ) )

   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : PrimitiveSchemaBuilder[ T ] =
       validate( validator +: otherValidators )

   def validate( validators : Iterable[ Validator[ T ] ] ) : PrimitiveSchemaBuilder[ T ] = copy( vals = validators.toSet )

   def examples( example : T, otherExamples : T* ) : PrimitiveSchemaBuilder[ T ] =
     examples( example +: otherExamples )

   def examples( examples : Seq[ T ] ) : PrimitiveSchemaBuilder[ T ] = copy( exs = examples )

   def deprecated : PrimitiveSchemaBuilder[ T ] = copy( dep = true )

   def build : Primitive[ T ] = Primitive[ T ](
       nm,
       desc,
       vals,
       exs,
       dep,
   )
}

object PrimitiveSchemaBuilder {
  def apply[ T ] : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]()
}

trait SchemaRebuilder[ T, S ] {
    type Builder

    def rebuild( schema : Schema.Aux[ T, S ] ) : Builder
}

object SchemaRebuilder {
    type Aux[ T, S, B ] = SchemaRebuilder[ T, S ] { type Builder = B }

    given productRebuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
        using
        ctx : CtxWrapTuplesConstraint[ Field, R, RV ],
    ) : SchemaRebuilder.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ], ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] ] = {
        new SchemaRebuilder[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] {
            type Builder = ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ]

            def rebuild( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ]) : Builder = {
                ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ](
                    schema.name,
                    schema.genericDescription,
                    schema.genericValidators,
                    schema.genericExamples,
                    schema.deprecated,
                    schema.shape.additionalFieldsSchema,
                    schema.shape.fieldDescriptions,
                    schema.shape.constructor,
                    schema.shape.deconstructor,
                )
            }
        }
    }

    import org.hungerford.generic.schema.product.field.Ineq

    given primitiveRebuilder[ T ](
        using
        ev : Ineq[ T, Nothing ]
    ) : SchemaRebuilder.Aux[ T, Unit, PrimitiveSchemaBuilder[ T ] ] = new SchemaRebuilder[ T, Unit ] {
        type Builder = PrimitiveSchemaBuilder[ T ]

        def rebuild( schema : Schema.Aux[ T, Unit ] ) : Builder = {
            PrimitiveSchemaBuilder[ T ](
                schema.name,
                schema.genericDescription,
                schema.genericValidators,
                schema.genericExamples,
                schema.deprecated,
            )
        }
    }
}
