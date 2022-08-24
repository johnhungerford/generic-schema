package org.hungerford.generic.schema

import org.hungerford.generic.schema.coproduct.{CoproductSchemaBuilder, CoproductShape}
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.{ProductDeriver, ProductSchemaBuilder, ProductShape}
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

    given productRebuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
		using
		ctx : CtxWrapTuplesConstraint[ Field.Tpe, R, RV ],
    ) : SchemaRebuilder.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] ] = {
        new SchemaRebuilder[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] {
            type Builder = ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ]

            def rebuild( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]) : Builder = {
                ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ](
                    schema.name,
                    schema.genericDescription,
                    schema.genericValidators,
                    schema.genericExamples,
                    schema.deprecated,
                    schema.shape.additionalFieldsSchema,
                    schema.shape.afExtractor,
                    schema.shape.fieldDescriptions,
                    schema.shape.constructor,
                )
            }
        }
    }

    given coproductRebuilder[ T, R <: Tuple, RV <: Tuple, D, DN ] : SchemaRebuilder[ T, CoproductShape[ T, R, RV, D, DN ] ] with {
        type Builder = CoproductSchemaBuilder[ T, R, D, DN ]

        def rebuild(
            schema : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
        ) : CoproductSchemaBuilder[ T, R, D, DN ] = {
            CoproductSchemaBuilder[ T, R, D, DN ](
                schema.name,
                schema.genericDescription,
                schema.genericValidators,
                schema.genericExamples,
                schema.deprecated,
                schema.shape.subtypeDescriptions,
            )
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
