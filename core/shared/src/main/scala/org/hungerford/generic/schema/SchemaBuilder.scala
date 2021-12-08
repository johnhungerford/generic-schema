package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.{CtxWrapTuplesConstraint, ProductDeriver, ProductSchemaBuilder, ProductShape, TupleIntLength}
import org.hungerford.generic.schema.validator.Validator

case class SchemaBuilder[ T ](
   private[ schema ] val desc : Option[ String ] = None,
   private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) {
   def description( description : String ) : SchemaBuilder[ T ] = copy( desc = Some( description ) )

   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : SchemaBuilder[ T ] =
       validate( validator +: otherValidators )

   def validate( validators : Iterable[ Validator[ T ] ] ) : SchemaBuilder[ T ] = copy( vals = validators.toSet )

   def primitive : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]( desc, vals )

   def buildPrimitive : Primitive[ T ] = Primitive[ T ]( desc, vals )

   def product : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ] =
       ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ](
           desc,
           vals,
           NoSchema,
           EmptyTuple,
           (),
           (),
       )

   def caseClass[ R <: Tuple, Rt <: Tuple, RVt <: Tuple ](
       implicit
       deriver : SchemaDeriver.Aux[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ] ],
       fieldsConstraint : CtxWrapTuplesConstraint[ Field, Rt, RVt ],
   ) : ProductSchemaBuilder[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ] = {
       ProductSchemaBuilder.from[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ]( deriver.derive )
   }
}

case class PrimitiveSchemaBuilder[ T ](
   private[ schema ] val desc : Option[ String ] = None,
   private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) {
   def description( description : String ) : PrimitiveSchemaBuilder[ T ] = copy( desc = Some( description ) )

   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : PrimitiveSchemaBuilder[ T ] =
       validate( validator +: otherValidators )

   def validate( validators : Iterable[ Validator[ T ] ] ) : PrimitiveSchemaBuilder[ T ] = copy( vals = validators.toSet )

   def build : Primitive[ T ] = Primitive[ T ](
       desc,
       vals,
   )
}

object SchemaBuilder {
   def apply[ T ] : SchemaBuilder[ T ] = SchemaBuilder[ T ]()

   def none : Schema.Aux[ Nothing, Unit ] = NoSchema

   def from[ T, S ](
       schema : Schema.Aux[ T, S ],
   )(
       using
       rb : SchemaRebuilder[ T, S ],
   ) : rb.Builder = rb.rebuild( schema )
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
                    schema.genericDescription,
                    schema.genericValidators,
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
                schema.genericDescription,
                schema.genericValidators,
            )
        }
    }
}
