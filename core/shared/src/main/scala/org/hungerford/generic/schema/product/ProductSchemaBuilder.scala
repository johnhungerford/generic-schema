package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{BuildableFieldBuilder, Field, FieldBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.{ComplexSchema, NoSchema, Primitive, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.selector.{AmbigSelector, ComponentRetriever, ComponentUpdater, FieldSelector, Selector}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

import scala.collection.immutable.NewVectorIterator

case class ProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
    private[ schema ] val nm : Option[ String ] = None,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ schema ] val exs : Seq[ T ] = Nil,
    private[ schema ] val dep : Boolean = false,
    private[ schema ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ schema ] val afe : AFE,
    private[ schema ] val fieldDescs : R,
    private[ schema ] val constr : C,
)(
    using
    fieldsConstraint : CtxWrapTuplesConstraint[ Field.Ctx[ T ], R, RV ],
) {
   def name( name : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = copy( nm = Some( name ) )
   def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = copy( desc = Some( description ) )
   def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] =
       validate ( validator +: otherValidators )
   def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = copy( vals = validators.toSet )
   def examples( example : T, otherExamples : T* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] =
     examples( example +: otherExamples )
   def examples( examples : Seq[ T ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = copy( exs = examples.toSeq )
   def deprecate : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = copy( dep = true )

   def addField[ F, N <: FieldName, S ](
       fd : Field.Aux[ T, F, N, S ],
   )(
       using
       fc : => CtxWrapTuplesConstraint[ Field.Ctx[ T ], Tuple.Concat[ R, Field.Aux[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
       uniq : UniqueFieldNames[ Tuple.Concat[ R, Field.Aux[ T, F, N, S ] *: EmptyTuple ] ],
   ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, Field.Aux[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ] = {
       val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
       copy[ T, Tuple.Concat[ R, Field.Aux[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, Unit, Unit ]( fieldDescs = newFieldDescs, constr = (), decons = () )
   }

   def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
       fieldName : N,
   )(
       using
       rm : FieldRemover.Aux[ N, R, NewR ],
       fc : => CtxWrapTuplesConstraint[ Field.Ctx[ T ], NewR, NewRV ],
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
       fr : FieldRetriever.Aux[ N, R, Field.Aux[ T, F, N, S ] ]
   ) : FieldUpdater[F, N, S, T, R, RV, AF, AFS, C ] = FieldUpdater[F, N, S, T, R, RV, AF, AFS, C ](
      fr.retrieve( fieldDescs ),
      nm,
      desc,
      vals,
      exs,
      dep,
      aftSch,
      fieldDescs,
      constr,
      decons,
    )

    def modifyComponent[ Sel <: Tuple, F, N <: FieldName, FS, Inner ](
        selector : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C ], Sel, Inner ],
    ) : FieldComponentUpdater[ T, R, RV, AF, AFS, C, F, N, FS, Sel, Inner ] = FieldComponentUpdater[ T, R, RV, AF, AFS, C, F, N, FS, Sel, cr.Inner ](
        this,
    )

   def additionalFields[ NewAF ](
       using
       afChoice : ConstrUpdateChoice[ RV, RV, AF, NewAF, C ],
   ) : AdditionalFieldsBuilder[ T, R, RV, AF, AFS, AFE, C, NewAF ] =
       AdditionalFieldsBuilder[ T, R, RV, AF, AFS, AFE, C, NewAF ]( this )

   def construct(
       using
       cType : ConstructorChooser[ T, RV, AF ],
   ) : ConstructorBuilder[ T, R, RV, AF, AFS, cType.Constr ] = {
       ConstructorBuilder[ T, R, RV, AF, AFS, cType.Constr ](
            nm,
            desc,
            vals,
            exs,
            dep,
            aftSch,
            fieldDescs,
            decons,
       )
   }

    def build(
       using
       uniq : => UniqueFieldNames[ R ],
       pc : ProductConstructor[ C, RV, AF, T ],
   ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C ] ] =
       ComplexSchema(
           ProductShape[ T, R, RV, AF, AFS, C ](
               fieldDescriptions = fieldDescs,
               additionalFieldsSchema = aftSch,
               constructor = constr,
           ),
           name = nm,
           genericDescription = desc,
           genericValidators = vals,
           genericExamples = exs,
           deprecated = dep,
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

case class ConstructorBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C ](
    private[ product ] val nm : Option[ String ] = None,
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val exs : Seq[ T ] = Nil,
    private[ product ] val dep : Boolean = false,
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val decons : DC,
)(
    using
    cType : ConstructorChooser.Aux[ T, RV, AF, C ],
    ctx : CtxWrapTuplesConstraint[ Field.Ctx[ T ], R, RV ],
) {
    def apply( constructor : C ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = {
        ProductSchemaBuilder[ T, R, RV, AF, AFS, C ](
            nm,
            desc,
            vals,
            exs,
            dep,
            aftSch,
            fieldDescs,
            constructor,
            decons,
        )
    }
}

trait ConstrUpdateChoice[ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C ] {
    type Constr

    def constructor( original : C ) : Constr
}

trait LowPriorityCUCs {
    given [ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C ] : ConstrUpdateChoice[ RV, NewRV, AF, NewAF, C ] with {
        type Constr = Unit

        override def constructor( original : C ) : Unit = ()
    }
}

object ConstrUpdateChoice extends LowPriorityCUCs {
    type Aux[ RV <: Tuple, NewRV <: Tuple, AF, NewAF, C, NewC ] = ConstrUpdateChoice[ RV, NewRV, AF, NewAF, C ] {
        type Constr = NewC
    }

    given[ RV <: Tuple, AF, C ] : ConstrUpdateChoice[ RV, RV, AF, AF, C ] with {
        type Constr = C

        override def constructor( original : C ) : C = original
    }
}

case class AdditionalFieldsBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, NewAF, NewC ](
   private[ product ] val builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ]
)(
   using
   fieldsConstraint : CtxWrapTuplesConstraint[ Field.Ctx[ T ], R, RV ],
   val afChoice : ConstrUpdateChoice.Aux[ RV, RV, AF, NewAF, C, NewC ],
) {
    def fromSchema[ S ](
        extract : T => NewAF,
    )(
        implicit schema : Schema.Aux[ NewAF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, T => NewAF, NewC ] = {
        ProductSchemaBuilder[ T, R, RV, NewAF, S, T => NewAF, NewC ](
            builder.nm,
            builder.desc,
            builder.vals,
            builder.exs,
            builder.dep,
            schema,
            extract,
            builder.fieldDescs,
            afChoice.constructor( builder.constr ),
        )
    }

    def primitive(
        extract: T => NewAF,
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, Unit, T => NewAF, NewC ] = {
        fromSchema[ Unit ]( extract )( Primitive[NewAF]() )
    }
}

case class FieldUpdater[ F, N <: FieldName, S, T, R <: Tuple, RV <: Tuple, AF, AFS, C ](
    private[ product ] val field : Field.Aux[ T, F, N, S ],
    private[ product ] val nm : Option[ String ] = None,
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val exs : Seq[ T ] = Nil,
    private[ product ] val dep : Boolean = false,
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : C,
) {
    def apply[ NewN <: FieldName, NewS, NewR <: Tuple ](
        builder: BuildableFieldBuilder[ F, N, S ] => Field.Aux[ T, F, NewN, NewS ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, F, NewN, NewS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C ] = {
        val startingBuilder = FieldBuilder.from( field )
        val newField = builder( startingBuilder )
        val newFieldDescriptions = fr.replace( fieldDescs, newField )

        ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C ](
            nm,
            desc,
            vals,
            exs,
            dep,
            aftSch,
            newFieldDescriptions,
            constr,
        )
    }
}

case class FieldComponentUpdater[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, F, N <: FieldName, FS, Sel <: Tuple, Inner ](
    builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ],
) {
    def apply[ NewInner, NewN <: FieldName, NewFS, NewR <: Tuple ](
        updater : Inner => NewInner,
    )(
        using
        fu : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
        cu : => ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C ], Sel, Inner, NewInner, ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C ] ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C ] = {
        cu.update( builder )( updater )
    }
}

object ProductSchemaBuilder {
   def apply[ T ] : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ] =
     ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ](
       fieldDescs = EmptyTuple,
       aftSch = NoSchema,
       constr = (),
     )

   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS, C ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ Field, R, RV ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, C ] = {
       ProductSchemaBuilder(
           schema.name,
           schema.genericDescription,
           schema.genericValidators,
           schema.genericExamples,
           schema.deprecated,
           schema.shape.additionalFieldsSchema,
           schema.shape.fieldDescriptions,
           schema.shape.constructor,
       )
   }
}
