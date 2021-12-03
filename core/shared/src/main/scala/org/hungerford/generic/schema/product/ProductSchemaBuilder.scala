package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{BuildableFieldDescriptionBuilder, FieldDescription, FieldDescriptionBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, FieldSelector, Selector}

import scala.collection.immutable.NewVectorIterator

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

   def updateField[ N <: FieldName, F, S ](
       fieldName : N,
   )(
       using
       fr : FieldRetriever.Aux[ N, R, FieldDescription.Aux[ F, N, S ] ]
   ) : FieldUpdater[F, N, S, T, R, RV, AF, AFS, C, DC ] = FieldUpdater[F, N, S, T, R, RV, AF, AFS, C, DC ](
      fr.retrieve( fieldDescs ),
      desc,
      vals,
      aftSch,
      fieldDescs,
      constr,
      decons,
    )

    def updateComponent[ N <: FieldName, Sel <: Tuple, F, FS ](
        selector : Selector[ FieldSelector[ N ] *: Sel ],
    )(
        using
        fr : FieldRetriever.Aux[ N, R, FieldDescription.Aux[ F, N, FS ] ],
        cr : ComponentRetriever[ FieldDescription.Aux[ F, N, FS ], Sel ],
    ) : FieldComponentUpdater[ T, R, RV, AF, AFS, C, DC, F, N, FS, Sel, cr.Inner ] = FieldComponentUpdater[ T, R, RV, AF, AFS, C, DC, F, N, FS, Sel, cr.Inner ](
        fr.retrieve( fieldDescs ),
        desc,
        vals,
        aftSch,
        fieldDescs,
        constr,
        decons,
    )

   def additionalFields[ F ](
       using
       afChoice : ConstrUpdateChoice[ RV, RV, AF, F, C, DC ],
   ) : AdditionalFieldsBuilder[ T, R, RV, AF, F, C, DC, afChoice.Constr, afChoice.Deconstr ] =
       AdditionalFieldsBuilder[ T, R, RV, AF, F, C, DC, afChoice.Constr, afChoice.Deconstr ](
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

trait EvenLowerestPriorityConstructorChoosers {
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

trait LowestPriorityConstructorChoosers extends EvenLowerestPriorityConstructorChoosers {
    given singleField[ T, F, AF ] : ConstructorChooser.Aux[
        T,
        F *: EmptyTuple,
        AF,
        (F, Map[ String, AF]) => T,
    ] = new ConstructorChooser[ T, F *: EmptyTuple, AF ] {
        type Constr = (F, Map[ String, AF]) => T
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

    given singleField[ T, F, AF ] : DeconstructorChooser.Aux[
        T,
        F *: EmptyTuple,
        AF,
        T => (F, Map[ String, AF ]),
    ] = new DeconstructorChooser[ T, F *: EmptyTuple, AF ] {
        type Deconstr = T => (F, Map[ String, AF ])
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

trait ConstrUpdateChoice[ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C, DC ] {
    type Constr
    type Deconstr

    def constructor( original : C ) : Constr
    def deconstructor( original : DC ) : Deconstr
}

trait LowPriorityCUCs {
    given [ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C, DC ] : ConstrUpdateChoice[ RV, NewRV, AF, NewAF, C, DC ] with {
        type Constr = Unit
        type Deconstr = Unit

        override def constructor( original : C ) : Unit = ()
        override def deconstructor( original : DC ) : Unit = ()
    }
}

object ConstrUpdateChoice extends LowPriorityCUCs {
    type Aux[ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C, DC, NewC, NewDC ] = ConstrUpdateChoice[ RV, NewRV, AF, NewAF, C, DC ] {
        type Constr = NewC
        type Deconstr = NewDC
    }

    given[ RV <: Tuple, AF, C, DC ] : ConstrUpdateChoice[ RV, RV, AF, AF, C, DC ] with {
        type Constr = C
        type Deconstr = DC

        override def constructor( original : C ) : C = original
        override def deconstructor( original : DC ) : DC = original
    }
}

case class AdditionalFieldsBuilder[ T, R <: Tuple, RV <: Tuple, AF, NewAF, C, DC, NewC, NewDC ](
   private[ product ] val desc : Option[ String ] = None,
   private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ product ] val fieldDescs : R,
   private[ product ] val constr : C,
   private[ product ] val decons : DC,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, R, RV ],
   val afChoice : ConstrUpdateChoice.Aux[ RV, RV, AF, NewAF, C, DC, NewC, NewDC ],
) {
    def fromSchema[ S ](
        implicit schema : Schema.Aux[ NewAF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, NewC, NewDC ] = {
        ProductSchemaBuilder[ T, R, RV, NewAF, S, NewC, NewDC ](
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
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, NewC, NewDC ] = {
        fromSchema( builder( SchemaBuilder[ NewAF ] ) )
    }
}

case class FieldUpdater[ F, N <: FieldName, S, T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
    private[ product ] val field : FieldDescription.Aux[ F, N, S ],
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : C,
    private[ product ] val decons : DC,
) {
    def apply[ NewN <: FieldName, NewS, NewR <: Tuple ](
        builder: BuildableFieldDescriptionBuilder[ F, N, S ] => FieldDescription.Aux[ F, NewN, NewS ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, NewS, NewR ],
        ctx : CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] = {
        val startingBuilder = FieldDescriptionBuilder.from( field )
        val newField = builder( startingBuilder )
        val newFieldDescriptions = fr.replace( fieldDescs, newField )

        ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ](
            desc,
            vals,
            aftSch,
            newFieldDescriptions,
            constr,
            decons,
        )
    }
}

case class FieldComponentUpdater[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, F, N <: FieldName, FS, Sel <: Tuple, Inner ](
    private[ product ] val field : FieldDescription.Aux[ F, N, FS ],
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : C,
    private[ product ] val decons : DC,
) {
    def apply[ NewInner, NewN <: FieldName, NewFS, NewR <: Tuple ](
        updater : Inner => NewInner,
    )(
        using
        cu : ComponentUpdater.Aux[ FieldDescription.Aux[ F, N, FS ], Sel, Inner, NewInner, FieldDescription.Aux[ F, NewN, NewFS ] ],
        fu : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] = {
        val newField = cu.update( field )( updater )
        val newFields = fu.replace( fieldDescs, newField )
        ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ](
            desc,
            vals,
            aftSch,
            newFields,
            constr,
            decons,
        )
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
