package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldName, UniqueFieldNames}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import org.hungerford.generic.schema.validator.Validator



case class ProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ product ] val fieldDescs : R,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( vals = validators.toSet )

   def addField[ F, N <: FieldName, S ](
       fd : FieldDescription.Aux[ F, N, S ],
   )(
        using
        fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ]
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       copy[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ]( desc, vals, aftSch, newFieldDescs )
   }

   def additionalFields[ F ] : AdditionalFieldsBuilder[ T, R, RV, F ] =
       AdditionalFieldsBuilder[ T, R, RV, F ](
           desc,
           vals,
           fieldDescs,
       )

   def construct(
       constructor : ( RV, Map[ String, AF ] ) => T,
   ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
       ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ](
           desc,
           vals,
           aftSch,
           fieldDescs,
           constructor,
       )

   def deconstruct(
       deconstructor : T => (RV, Map[ String, AF ]),
   ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
       ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ](
           desc,
           vals,
           aftSch,
           fieldDescs,
           deconstructor,
       )
}

case class ProductSchemaBuilderWithConstructor[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ product ] val fieldDescs : R,
   private[ product ] val constr : ( RV, Map[ String, AF ] ) => T
)(
   implicit
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def description( description : String ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
       copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
       copy( vals = validators.toSet )

   def construct(
       constructor : (RV, Map[ String, AF ]) => T,
   ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
       copy( constr = constructor )

   def deconstruct(
       deconstructor : T => (RV, Map[ String, AF ]),
   ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
           desc,
           vals,
           aftSch,
           fieldDescs,
           constr,
           deconstructor,
       )
}

case class ProductSchemaBuilderWithDeconstructor[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ product ] val fieldDescs : R,
   private[ product ] val deconstr : T => (RV, Map[ String, AF ])
)(
   implicit
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def description( description : String ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
       copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
       copy( vals = validators.toSet )

   def deconstruct(
       deconstructor : T => (RV, Map[ String, AF ]),
   ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
       copy( deconstr = deconstructor )

   def construct(
       constructor : ( RV, Map[ String, AF ] ) => T,
   ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
           desc,
           vals,
           aftSch,
           fieldDescs,
           constructor,
           deconstr,
       )
}

case class BuildableProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ product ] val fieldDescs : R,
   private[ product ] val constr : ( RV, Map[ String, AF ] ) => T,
   private[ product ] val deconstr : T => (RV, Map[ String, AF ]),
)(
   implicit
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def description( description : String ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
       copy( vals = validators.toSet )

   def build(
       using
       lengther : TupleIntLength[ R ],
       uniq : => UniqueFieldNames[ R ],
   ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS ] ] =
       ComplexSchema(
           ProductShape[ T, R, RV, AF, AFS ](
               fieldDescriptions = fieldDescs,
               additionalFieldsSchema = aftSch,
               constructor = constr,
               deconstructor = deconstr,
           ),
           genericDescription = desc,
           genericValidators = vals,
       )
}

case class AdditionalFieldsBuilder[ T, R <: Tuple, RV <: Tuple, AF ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val fieldDescs : R,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def fromSchema[ S ](
       implicit schema : Schema.Aux[ AF, S ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, S ] = {
       ProductSchemaBuilder[ T, R, RV, AF, S ](
           desc,
           vals,
           schema,
           fieldDescs,
       )
   }

   def buildSchema[ S ](
       builder : SchemaBuilder[ AF ] => Schema.Aux[ AF, S ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, S ] = {
       fromSchema( builder( SchemaBuilder[ AF ] ) )
   }
}

object ProductSchemaBuilder {
   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
   ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] = {
       BuildableProductSchemaBuilder(
           schema.genericDescription,
           schema.genericValidators,
           schema.shape.additionalFieldsSchema,
           schema.shape.fieldDescriptions,
           schema.shape.constructor,
           schema.shape.deconstructor
       )
   }
}
