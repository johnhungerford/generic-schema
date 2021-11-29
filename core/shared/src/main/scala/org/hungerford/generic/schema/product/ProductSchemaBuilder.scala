package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, UniqueFieldNames, BuildableFieldDescriptionBuilder}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}

//case class ProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
//    private[ product ] val desc : Option[ String ] = None,
//    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
//    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
//    private[ product ] val fieldDescs : R,
//    private[ product ] val constr : C,
//    private[ product ] val decons : DC,
//)(
//    using
//    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
//) {
//    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( desc = Some( description ) )
//    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] =
//        validate ( validator +: otherValidators )
//    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( vals = validators.toSet )
//
//    def addField[ F, N <: FieldName, S ](
//        fd : FieldDescription.Aux[ F, N, S ],
//    )(
//        using
//        fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
//        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
//    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ] = {
//        val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
//        copy[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ]( desc, vals, aftSch, newFieldDescs, (), () )
//    }
//
//    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
//        fieldName : N,
//    )(
//        using
//        rm : FieldRemover.Aux[ N, R, NewR ],
//        fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, Unit, Unit ] = {
//        val newFields = rm.remove( fieldDescs )
//        copy[ T, NewR, NewRV, AF, AFS ](
//            fieldDescs = newFields,
//            constr = (),
//            decons = (),
//        )
//    }
//
//    def additionalFields[ F ] : AdditionalFieldsBuilder[ T, R, RV, F ] =
//        AdditionalFieldsBuilder[ T, R, RV, F ](
//            desc,
//            vals,
//            fieldDescs,
//        )
//
//    def construct(
//        constructor : ( RV, Map[ String, AF ] ) => T,
//    ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, (RV, Map[ String, AF ]) => T, DC ] =
//        copy[ T, R, RV, AF, AFS, (RV, Map[ String, AF ]) => T, DC ](
//            constr = constructor,
//            ProductSchemaBuilder)
//
//    def deconstruct(
//        deconstructor : T => (RV, Map[ String, AF ]),
//    ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, T => (RV, Map[ String, AF ] ] =
//        copy[ T, R, RV, AF, AFS, C, T => (RV, Map[ String, AF ] ](
//            decons = deconstructor,
//        )
//}

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
        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       copy[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ]( desc, vals, aftSch, newFieldDescs )
   }

    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
        fieldName : N,
    )(
        using
        rm : FieldRemover.Aux[ N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFields = rm.remove( fieldDescs )
        copy[ T, NewR, NewRV, AF, AFS ](
            fieldDescs = newFields,
        )
    }

    def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
        fieldName : N,
    )(
        newField : FieldDescription.Aux[ F, NewN, S ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = fr.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
        fieldName : OldN,
        buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
    )(
        using
        frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
        frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS ] = {
        val oldField = frt.retrieve( fieldDescs )
        val fieldBuilder = FieldDescriptionBuilder.from( oldField )
        val newField = buildField( fieldBuilder )
        val newFieldDescs = frp.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
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

    def addField[ F, N <: FieldName, S ](
        fd : FieldDescription.Aux[ F, N, S ],
    )(
        using
        fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
        val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
        ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
        fieldName : N,
    )(
        using
        rm : FieldRemover.Aux[ N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = rm.remove( fieldDescs )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
        fieldName : N,
        newField : FieldDescription.Aux[ F, NewN, S ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = fr.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
        fieldName : OldN,
        buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
    )(
        using
        frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
        frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ProductSchemaBuilderWithConstructor[ T, NewR, RV, AF, AFS ] = {
        val oldField = frt.retrieve( fieldDescs )
        val fieldBuilder = FieldDescriptionBuilder.from( oldField )
        val newField = buildField( fieldBuilder )
        val newFieldDescs = frp.replace( fieldDescs, newField )
        ProductSchemaBuilderWithConstructor[ T, NewR, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
            constr,
        )
    }

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

   def addField[ F, N <: FieldName, S ](
       fd : FieldDescription.Aux[ F, N, S ],
   )(
       using
       fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
       uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
           desc,
           vals,
           aftSch,
           newFieldDescs,
       )
   }

    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
        fieldName : N,
    )(
        using
        rm : FieldRemover.Aux[ N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = rm.remove( fieldDescs )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
        fieldName : N,
        newField : FieldDescription.Aux[ F, NewN, S ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = fr.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
        fieldName : OldN,
        buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
    )(
        using
        frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
        frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ProductSchemaBuilderWithDeconstructor[ T, NewR, RV, AF, AFS ] = {
        val oldField = frt.retrieve( fieldDescs )
        val fieldBuilder = FieldDescriptionBuilder.from( oldField )
        val newField = buildField( fieldBuilder )
        val newFieldDescs = frp.replace( fieldDescs, newField )
        ProductSchemaBuilderWithDeconstructor[ T, NewR, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
            deconstr,
        )
    }

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

   def addField[ F, N <: FieldName, S ](
       fd : FieldDescription.Aux[ F, N, S ],
   )(
       using
       fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
       uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
       val newFieldDescs = fieldDescs ++ ( fd *: EmptyTuple )
       ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
           desc,
           vals,
           aftSch,
           newFieldDescs,
       )
   }

    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
        fieldName : N,
    )(
        using
        rm : FieldRemover.Aux[ N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = rm.remove( fieldDescs )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
        fieldName : N,
        newField : FieldDescription.Aux[ F, NewN, S ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
        val newFieldDescs = fr.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
        )
    }

    def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
        fieldName : OldN,
    )(
        buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
    )(
        using
        frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
        frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
        ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : BuildableProductSchemaBuilder[ T, NewR, RV, AF, AFS ] = {
        val oldField = frt.retrieve( fieldDescs )
        val fieldBuilder = FieldDescriptionBuilder.from( oldField )
        val newField = buildField( fieldBuilder )
        val newFieldDescs = frp.replace( fieldDescs, newField )
        BuildableProductSchemaBuilder[ T, NewR, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            newFieldDescs,
            constr,
            deconstr,
        )
    }

   def build(
       using
       lengther : TupleIntLength[ R ],
       uniq : => UniqueFieldNames[ R ],
   ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, (RV, Map[ String, AF]) => T, (RV, Map[ String, AF ]) ] ] =
       ComplexSchema(
           ProductShape[ T, R, RV, AF, AFS, (RV, Map[ String, AF]) => T, (RV, Map[ String, AF ]) ](
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
   )(
       using
       prodConstr : ProductConstructor[ (RV, Map[ String, AF]) => T, RV, AF, T ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, S ] = {
       fromSchema( builder( SchemaBuilder[ AF ] ) )
   }
}



object ProductSchemaBuilder {
   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
       prodConstr : ProductConstructor[ C, RV, AF, T ],
       prodDeconstr : ProductDeconstructor[ T, RV, AF, DC ]
   ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] = {
       BuildableProductSchemaBuilder(
           schema.genericDescription,
           schema.genericValidators,
           schema.shape.additionalFieldsSchema,
           schema.shape.fieldDescriptions,
           prodConstr.convert( schema.shape.constructor ),
           prodDeconstr.convert( schema.shape.deconstructor ),
       )
   }
}
