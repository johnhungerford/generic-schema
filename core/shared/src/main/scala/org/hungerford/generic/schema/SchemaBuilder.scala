package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.FieldDescription
import org.hungerford.generic.schema.product.{BuildableProductSchemaBuilder, CtxWrapTuplesConstraint, TupleIntLength, ProductDeriver, ProductShape, ProductSchemaBuilder}
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

   def product : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit ] =
       ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit ](
           desc,
           vals,
           NoSchema,
           EmptyTuple,
       )

   def caseClass[ R <: Tuple, Rt <: Tuple, RVt <: Tuple ](
       implicit
       deriver : SchemaDeriver.Aux[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, RVt => T ] ],
       fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, Rt, RVt ],
   ) : BuildableProductSchemaBuilder[ T, Rt, RVt, Nothing, Unit ] = {
       ProductSchemaBuilder.from[ T, Rt, RVt, Nothing, Unit, RVt => T ]( deriver.derive )
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
}
