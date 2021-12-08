package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{BuildableFieldBuilder, Field, FieldBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.selector.{AmbigSelector, ComponentRetriever, ComponentUpdater, FieldSelector, Selector}

import scala.collection.immutable.NewVectorIterator

case class ProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
   private[ schema ] val desc : Option[ String ] = None,
   private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private[ schema ] val aftSch : Schema.Aux[ AF, AFS ],
   private[ schema ] val fieldDescs : R,
   private[ schema ] val constr : C,
   private[ schema ] val decons : DC,
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ Field, R, RV ],
) {
   def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] = copy( vals = validators.toSet )

   def addField[ F, N <: FieldName, S ](
       fd : Field.Aux[ F, N, S ],
   )(
       using
       fc : => CtxWrapTuplesConstraint[ Field, Tuple.Concat[ R, Field.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
       uniq : UniqueFieldNames[ Tuple.Concat[ R, Field.Aux[ F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, Field.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       copy[ T, Tuple.Concat[ R, Field.Aux[ F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ]( desc, vals, aftSch, newFieldDescs, (), () )
   }

   def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
       fieldName : N,
   )(
       using
       rm : FieldRemover.Aux[ N, R, NewR ],
       fc : => CtxWrapTuplesConstraint[ Field, NewR, NewRV ],
   ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, Unit, Unit ] = {
       val newFields = rm.remove( fieldDescs )
       copy[ T, NewR, NewRV, AF, AFS, Unit, Unit ](
           fieldDescs = newFields,
           constr = (),
           decons = (),
       )
   }

   def rebuildField[ N <: FieldName, F, S ](
       fieldName : N,
   )(
       using
       fr : FieldRetriever.Aux[ N, R, Field.Aux[ F, N, S ] ]
   ) : FieldUpdater[F, N, S, T, R, RV, AF, AFS, C, DC ] = FieldUpdater[F, N, S, T, R, RV, AF, AFS, C, DC ](
      fr.retrieve( fieldDescs ),
      desc,
      vals,
      aftSch,
      fieldDescs,
      constr,
      decons,
    )

    def modifyComponent[ Sel <: Tuple, F, N <: FieldName, FS, Inner ](
        selector : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], Sel, Inner ],
    ) : FieldComponentUpdater[ T, R, RV, AF, AFS, C, DC, F, N, FS, Sel, Inner ] = FieldComponentUpdater[ T, R, RV, AF, AFS, C, DC, F, N, FS, Sel, cr.Inner ](
        this,
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
    ctx : CtxWrapTuplesConstraint[ Field, R, RV ],
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
    ctx : CtxWrapTuplesConstraint[ Field, R, RV ],
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
   fieldsConstraint : CtxWrapTuplesConstraint[ Field, R, RV ],
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
    private[ product ] val field : Field.Aux[ F, N, S ],
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : C,
    private[ product ] val decons : DC,
) {
    def apply[ NewN <: FieldName, NewS, NewR <: Tuple ](
        builder: BuildableFieldBuilder[ F, N, S ] => Field.Aux[ F, NewN, NewS ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, NewS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] = {
        val startingBuilder = FieldBuilder.from( field )
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
    builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ],
) {
    def apply[ NewInner, NewN <: FieldName, NewFS, NewR <: Tuple ](
        updater : Inner => NewInner,
    )(
        using
        fu : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
        cu : => ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], Sel, Inner, NewInner, ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] = {
        cu.update( builder )( updater )
    }
}

object ProductSchemaBuilder {
   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ Field, R, RV ],
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
