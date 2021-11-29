package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, UniqueFieldNames, BuildableFieldDescriptionBuilder}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}

case class ProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ product ] val fieldDescs : R,
   private[ product ] val constr : C,
   private[ product ] val decons : DC,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
   def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( vals = validators.toSet )

   def addField[ F, N <: FieldName, S ](
       fd : FieldDescription.Aux[ F, N, S ],
   )(
       using
       fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
       uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       copy[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ]( desc, vals, aftSch, newFieldDescs, (), () )
   }

   def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
       fieldName : N,
   )(
       using
       rm : FieldRemover.Aux[ N, R, NewR ],
       fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
   ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, Unit, Unit ] = {
       val newFields = rm.remove( fieldDescs )
       copy[ T, NewR, NewRV, AF, AFS, Unit, Unit ](
           fieldDescs = newFields,
           constr = (),
           decons = (),
       )
   }

   def additionalFields[ F ](
       using
       afChoice : AFChooser[ AF, F, C, DC ],
   ) : AdditionalFieldsBuilder[ T, R, RV, AF, F, C, DC ] =
       AdditionalFieldsBuilder[ T, R, RV, AF, F, C, DC ](
           desc,
           vals,
           fieldDescs,
           constr,
           decons,
       )

   def construct(
       using
       cType : ConstructorChooser[ T, RV, AF ],
   ) : ConstructorBuilder[ T, R, RV, AF, AFS, cType.Constr, DC ] = {
       ConstructorBuilder[ T, R, RV, AF, AFS, cType.Constr, DC ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            decons,
       )
   }

   def deconstruct(
       using
       dcType : DeconstructorChooser[ T, RV, AF ],
   ) : DeconstructorBuilder[ T, R, RV, AF, AFS, C, dcType.Deconstr ] = {
       DeconstructorBuilder[ T, R, RV, AF, AFS, C, dcType.Deconstr ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            constr,
       )
   }
    
    def build(
       using
       lengther : TupleIntLength[ R ],
       uniq : => UniqueFieldNames[ R ],
       pc : ProductConstructor[ C, RV, AF, T ],
       pdc : ProductDeconstructor[ DC, RV, AF, T ],
   ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] =
       ComplexSchema(
           ProductShape[ T, R, RV, AF, AFS, C, DC ](
               fieldDescriptions = fieldDescs,
               additionalFieldsSchema = aftSch,
               constructor = constr,
               deconstructor = decons,
           ),
           genericDescription = desc,
           genericValidators = vals,
       )
}

trait ConstructorChooser[ T, RV, AF ] {
    type Constr
}

trait LowestPriorityConstructorChoosers {
    given singleField[ T, F, AF ] : ConstructorChooser.Aux[
        T,
        F *: EmptyTuple,
        AF,
        (F, Map[ String, AF]) => T,
    ] = new ConstructorChooser[ T, F *: EmptyTuple, AF ] {
        type Constr = (F, Map[ String, AF]) => T
    }

    given multiField[ T, RV <: Tuple, AF ] : ConstructorChooser.Aux[
        T,
        RV,
        AF,
        (RV, Map[ String, AF ]) => T,
    ] = new ConstructorChooser[ T, RV, AF ] {
        type Constr =
            (RV, Map[ String, AF ]) => T
    }
}

trait LowPriorityConstructorChoosers extends LowestPriorityConstructorChoosers {
    given multiFieldNoAF[ T, RV <: Tuple ] : ConstructorChooser.Aux[
        T,
        RV,
        Nothing,
        RV => T,
    ] = new ConstructorChooser[ T, RV, Nothing ] {
        type Constr = RV => T
    }
}

object ConstructorChooser extends LowPriorityConstructorChoosers {
    type Aux[ T, RV <: Tuple, AF, C ] = ConstructorChooser[ T, RV, AF ] { type Constr = C }

    given noField[ T, AF ] : ConstructorChooser.Aux[
        T,
        EmptyTuple,
        AF,
        Map[ String, AF ] => T,
    ] = new ConstructorChooser[ T, EmptyTuple, AF ] {
        type Constr = Map[ String, AF ] => T
    }

    given singleFieldNoAF[ T, F ] : ConstructorChooser.Aux[
        T,
        F *: EmptyTuple,
        Nothing,
        F => T,
    ] = new ConstructorChooser[ T, F *: EmptyTuple, Nothing ] {
        type Constr = F => T
    }
}

case class ConstructorBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val decons : DC,
)(
    using
    cType : ConstructorChooser.Aux[ T, RV, AF, C ],
    ctx : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
    def apply( constructor : C ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = {
        ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            constructor,
            decons,
        )
    }
}

trait DeconstructorChooser[ T, RV <: Tuple, AF ] {
    type Deconstr
}

trait LowestPriorityDeconstructorChoosers {
    given standard[ T, RV <: Tuple, AF ] : DeconstructorChooser.Aux[
        T,
        RV,
        AF,
        T => (RV, Map[ String, AF ]),
    ] = new DeconstructorChooser[ T, RV, AF ] {
        type Deconstr = T => (RV, Map[ String, AF ])
    }
}

trait LowPriorityDeconstructorChoosers extends LowestPriorityDeconstructorChoosers {
    given multiFieldNoAF[ T, RV <: Tuple ] : DeconstructorChooser.Aux[
        T,
        RV,
        Nothing,
        T => RV,
    ] = new DeconstructorChooser[ T, RV, Nothing ] {
        type Deconstr = T => RV
    }
}

object DeconstructorChooser extends LowPriorityDeconstructorChoosers {
    type Aux[ T, RV <: Tuple, AF, DC ] = DeconstructorChooser[ T, RV, AF ] { type Deconstr = DC }

    given singleFieldNoAF[ T, F ] : DeconstructorChooser.Aux[
        T,
        F *: EmptyTuple,
        Nothing,
        T => F,
    ] = new DeconstructorChooser[ T, F *: EmptyTuple, Nothing ] {
        type Deconstr = T => F
    }
}

case class DeconstructorBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : C,
)(
    using
    dcType : DeconstructorChooser.Aux[ T, RV, AF, DC ],
    ctx : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
) {
    def apply( deconstructor : DC ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = {
        ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            constr,
            deconstructor,
        )
    }
}

trait AFChooser[ AF, NewAF, C, DC ] {
    type Constr
    type Deconstr

    def constructor( original : C ) : Constr
    def deconstructor( original : DC ) : Deconstr
}

trait LowPriorityAFChoosers {
    given [ AF, NewAF, C, DC ] : AFChooser[ AF, NewAF, C, DC ] with {
        type Constr = Unit
        type Deconstr = Unit

        override def constructor( original : C ) : Unit = ()
        override def deconstructor( original : DC ) : Unit = ()
    }
}

object AFChooser extends LowPriorityAFChoosers {
    given[ AF, C, DC ] : AFChooser[ AF, AF, C, DC ] with {
        type Constr = C
        type Deconstr = DC

        override def constructor( original : C ) : C = original
        override def deconstructor( original : DC ) : DC = original
    }
}

case class AdditionalFieldsBuilder[ T, R <: Tuple, RV <: Tuple, AF, NewAF, C, DC ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val fieldDescs : R,
   private[ product ] val constr : C,
   private[ product ] val decons : DC,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
   val afChoice : AFChooser[ AF, NewAF, C, DC ],
) {
    type Constr = afChoice.Constr
    type Deconstr = afChoice.Deconstr

    def fromSchema[ S ](
        implicit schema : Schema.Aux[ NewAF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, Constr, Deconstr ] = {
        ProductSchemaBuilder[ T, R, RV, NewAF, S, Constr, Deconstr ](
            desc,
            vals,
            schema,
            fieldDescs,
            afChoice.constructor( constr ),
            afChoice.deconstructor( decons ),
        )
    }

    def buildSchema[ S ](
        builder : SchemaBuilder[ NewAF ] => Schema.Aux[ NewAF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, Constr, Deconstr ] = {
        fromSchema( builder( SchemaBuilder[ NewAF ] ) )
    }
}

object ProductSchemaBuilder {
   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = {
       ProductSchemaBuilder(
           schema.genericDescription,
           schema.genericValidators,
           schema.shape.additionalFieldsSchema,
           schema.shape.fieldDescriptions,
           schema.shape.constructor,
           schema.shape.deconstructor,
       )
   }
}

// case class ProductSchemaBuilder1[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
//    private[ product ] val desc : Option[ String ] = None,
//    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
//    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
//    private[ product ] val fieldDescs : R,
// )(
//    using
//    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
// ) {
//    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( desc = Some( description ) )
//    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        validate ( validator +: otherValidators )
//    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( vals = validators.toSet )

//    def addField[ F, N <: FieldName, S ](
//        fd : FieldDescription.Aux[ F, N, S ],
//    )(
//         using
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
//         uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
//    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
//        val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
//        copy[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ]( desc, vals, aftSch, newFieldDescs )
//    }

//     def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
//         fieldName : N,
//     )(
//         using
//         rm : FieldRemover.Aux[ N, R, NewR ],
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFields = rm.remove( fieldDescs )
//         copy[ T, NewR, NewRV, AF, AFS ](
//             fieldDescs = newFields,
//         )
//     }

//     def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
//         fieldName : N,
//     )(
//         newField : FieldDescription.Aux[ F, NewN, S ],
//     )(
//         using
//         fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = fr.replace( fieldDescs, newField )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
//         fieldName : OldN,
//         buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
//     )(
//         using
//         frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
//         frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
//     ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS ] = {
//         val oldField = frt.retrieve( fieldDescs )
//         val fieldBuilder = FieldDescriptionBuilder.from( oldField )
//         val newField = buildField( fieldBuilder )
//         val newFieldDescs = frp.replace( fieldDescs, newField )
//         ProductSchemaBuilder[ T, NewR, RV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//    def additionalFields[ F ] : AdditionalFieldsBuilder[ T, R, RV, F ] =
//        AdditionalFieldsBuilder[ T, R, RV, F ](
//            desc,
//            vals,
//            fieldDescs,
//        )

//    def construct(
//        constructor : ( RV, Map[ String, AF ] ) => T,
//    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
//        ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            fieldDescs,
//            constructor,
//        )

//    def deconstruct(
//        deconstructor : T => (RV, Map[ String, AF ]),
//    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
//        ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            fieldDescs,
//            deconstructor,
//        )
// }

// case class ProductSchemaBuilderWithConstructor[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
//    private[ product ] val desc : Option[ String ] = None,
//    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
//    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
//    private[ product ] val fieldDescs : R,
//    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T
// )(
//    implicit
//    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
// ) {
//    def description( description : String ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
//        copy( desc = Some( description ) )
//    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
//        validate ( validator +: otherValidators )
//    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
//        copy( vals = validators.toSet )

//     def addField[ F, N <: FieldName, S ](
//         fd : FieldDescription.Aux[ F, N, S ],
//     )(
//         using
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
//         uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
//     ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
//         val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
//         ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
//         fieldName : N,
//     )(
//         using
//         rm : FieldRemover.Aux[ N, R, NewR ],
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = rm.remove( fieldDescs )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
//         fieldName : N,
//         newField : FieldDescription.Aux[ F, NewN, S ],
//     )(
//         using
//         fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = fr.replace( fieldDescs, newField )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
//         fieldName : OldN,
//         buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
//     )(
//         using
//         frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
//         frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
//     ) : ProductSchemaBuilderWithConstructor[ T, NewR, RV, AF, AFS ] = {
//         val oldField = frt.retrieve( fieldDescs )
//         val fieldBuilder = FieldDescriptionBuilder.from( oldField )
//         val newField = buildField( fieldBuilder )
//         val newFieldDescs = frp.replace( fieldDescs, newField )
//         ProductSchemaBuilderWithConstructor[ T, NewR, RV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//             constr,
//         )
//     }

//    def construct(
//        constructor : (RV, Map[ String, AF ]) => T,
//    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
//        copy( constr = constructor )

//    def deconstruct(
//        deconstructor : T => (RV, Map[ String, AF ]),
//    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            fieldDescs,
//            constr,
//            deconstructor,
//        )
// }

// case class ProductSchemaBuilderWithDeconstructor[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
//    private[ product ] val desc : Option[ String ] = None,
//    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
//    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
//    private[ product ] val fieldDescs : R,
//    private[ product ] val deconstr : T => (RV, Map[ String, AF ])
// )(
//    implicit
//    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
// ) {
//    def description( description : String ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
//        copy( desc = Some( description ) )
//    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
//        validate ( validator +: otherValidators )
//    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
//        copy( vals = validators.toSet )

//    def addField[ F, N <: FieldName, S ](
//        fd : FieldDescription.Aux[ F, N, S ],
//    )(
//        using
//        fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
//        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
//    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
//        val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
//        ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            newFieldDescs,
//        )
//    }

//     def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
//         fieldName : N,
//     )(
//         using
//         rm : FieldRemover.Aux[ N, R, NewR ],
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = rm.remove( fieldDescs )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
//         fieldName : N,
//         newField : FieldDescription.Aux[ F, NewN, S ],
//     )(
//         using
//         fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = fr.replace( fieldDescs, newField )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
//         fieldName : OldN,
//         buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
//     )(
//         using
//         frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
//         frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
//     ) : ProductSchemaBuilderWithDeconstructor[ T, NewR, RV, AF, AFS ] = {
//         val oldField = frt.retrieve( fieldDescs )
//         val fieldBuilder = FieldDescriptionBuilder.from( oldField )
//         val newField = buildField( fieldBuilder )
//         val newFieldDescs = frp.replace( fieldDescs, newField )
//         ProductSchemaBuilderWithDeconstructor[ T, NewR, RV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//             deconstr,
//         )
//     }

//    def deconstruct(
//        deconstructor : T => (RV, Map[ String, AF ]),
//    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
//        copy( deconstr = deconstructor )

//    def construct(
//        constructor : ( RV, Map[ String, AF ] ) => T,
//    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            fieldDescs,
//            constructor,
//            deconstr,
//        )
// }

// case class BuildableProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS ](
//    private[ product ] val desc : Option[ String ] = None,
//    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
//    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
//    private[ product ] val fieldDescs : R,
//    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T,
//    private[ product ] val deconstr : T => (RV, Map[ String, AF ]),
// )(
//    implicit
//    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
// ) {
//    def description( description : String ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        copy( desc = Some( description ) )
//    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        validate ( validator +: otherValidators )
//    def validate( validators : Iterable[ Validator[ T ] ] ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
//        copy( vals = validators.toSet )

//    def addField[ F, N <: FieldName, S ](
//        fd : FieldDescription.Aux[ F, N, S ],
//    )(
//        using
//        fc : => CtxWrapTuplesConstraint[ FieldDescription, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
//        uniq : UniqueFieldNames[ Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ] ],
//    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ] = {
//        val newFieldDescs = fieldDescs ++ ( fd *: EmptyTuple )
//        ProductSchemaBuilder[ T, Tuple.Concat[ R, FieldDescription.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS ](
//            desc,
//            vals,
//            aftSch,
//            newFieldDescs,
//        )
//    }

//     def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
//         fieldName : N,
//     )(
//         using
//         rm : FieldRemover.Aux[ N, R, NewR ],
//         fc : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = rm.remove( fieldDescs )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def replaceField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple, F, NewN <: FieldName, S ](
//         fieldName : N,
//         newField : FieldDescription.Aux[ F, NewN, S ],
//     )(
//         using
//         fr : FieldReplacer.Aux[ N, R, F, NewN, S, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, NewRV ],
//     ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ] = {
//         val newFieldDescs = fr.replace( fieldDescs, newField )
//         ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//         )
//     }

//     def updateField[ F, OldN <: FieldName, OldS, NewN <: FieldName, NewS, NewR <: Tuple ](
//         fieldName : OldN,
//     )(
//         buildField : BuildableFieldDescriptionBuilder[ F, OldN, OldS ] => FieldDescription.Aux[ F, NewN, NewS ],
//     )(
//         using
//         frt : FieldRetriever.Aux[ OldN, R, FieldDescription.Aux[ F, OldN, OldS ] ],
//         frp : FieldReplacer.Aux[ OldN, R, F, NewN, NewS, NewR ],
//         ctx : => CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
//     ) : BuildableProductSchemaBuilder[ T, NewR, RV, AF, AFS ] = {
//         val oldField = frt.retrieve( fieldDescs )
//         val fieldBuilder = FieldDescriptionBuilder.from( oldField )
//         val newField = buildField( fieldBuilder )
//         val newFieldDescs = frp.replace( fieldDescs, newField )
//         BuildableProductSchemaBuilder[ T, NewR, RV, AF, AFS ](
//             desc,
//             vals,
//             aftSch,
//             newFieldDescs,
//             constr,
//             deconstr,
//         )
//     }

// }


