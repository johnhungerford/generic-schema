package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, FieldName, FieldRemover, FieldReplacer, FieldRetriever, FieldTypeRemover, UniqueFieldNames}
import org.hungerford.generic.schema.{ComplexSchema, NoSchema, Primitive, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.selector.{AmbigSelector, ComponentRetriever, ComponentUpdater, FieldSelector, Selector, TypeSelector}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Nat}

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
    fieldsConstraint : CtxWrapTuplesConstraint[ Field.Of, R, RV ],
) {
    def name( name : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = copy( nm = Some( name ) )
    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] =
        validate ( validator +: otherValidators )

    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = copy( vals = validators.toSet )
    def examples( example : T, otherExamples : T* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = examples( example +: otherExamples )
    def examples( examples : Seq[ T ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = copy( exs = examples.toSeq )
    def deprecate : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = copy( dep = true )
    def addField[ F, N <: FieldName, S ](
        fd : Field[ T, F, N, S ],
    )(
        using
        fc : => CtxWrapTuplesConstraint[ Field.Of, Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
        uniq : UniqueFieldNames[ Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ] ],
    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, AFE, Unit ] = {
        val newFieldDescs = fieldDescs ++ (fd *: EmptyTuple)
        copy[ T, Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, AFE, Unit ]( fieldDescs = newFieldDescs, constr = () )
    }

    def buildField[ F ] : ProductFieldBuilder[ T, F, R, RV, AF, AFS, AFE, C ] =
        ProductFieldBuilder[ T, F, R, RV, AF, AFS, AFE, C ]( this )

    def removeField[ N <: FieldName, NewR <: Tuple, NewRV <: Tuple ](
        fieldName : N,
    )(
        using
        rm : FieldRemover.Aux[ N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ Field.Of, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, AFE, Unit ] = {
        val newFields = rm.remove( fieldDescs )
        copy[ T, NewR, NewRV, AF, AFS, AFE, Unit ](
            fieldDescs = newFields,
            constr = (),
        )
    }

    def removeField[ F, N <: Nat, NewR <: Tuple, NewRV <: Tuple ](
        typeSelector : TypeSelector[ F, N ],
    )(
        using
        rm : FieldTypeRemover.Aux[ F, N, R, NewR ],
        fc : => CtxWrapTuplesConstraint[ Field.Of, NewR, NewRV ],
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, AFE, Unit ] = {
        val newFields = rm.remove( fieldDescs )
        copy[ T, NewR, NewRV, AF, AFS, AFE, Unit ](
            fieldDescs = newFields,
            constr = (),
        )
    }

    def rebuildField[ N <: FieldName, F, S ](
        fieldName : N,
    )(
        using
        fr : FieldRetriever.Aux[ N, R, Field[ T, F, N, S ] ]
    ) : FieldUpdater[F, N, S, T, R, RV, AF, AFS, AFE, C ] = FieldUpdater[F, N, S, T, R, RV, AF, AFS, AFE, C ](
        fr.retrieve( fieldDescs ),
        this
    )

    def modifyComponent[ Sel <: Tuple, F, N <: FieldName, FS, Inner ](
        selector : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], Sel, Inner ],
    ) : FieldComponentUpdater[ T, R, RV, AF, AFS, AFE, C, F, N, FS, Sel, Inner ] = FieldComponentUpdater[ T, R, RV, AF, AFS, AFE, C, F, N, FS, Sel, cr.Inner ](
        this,
    )

   def additionalFields[ NewAF ](
       using
       afChoice : ConstrUpdateChoice[ RV, RV, AF, NewAF, C ],
   ) : AdditionalFieldsBuilder[ T, R, RV, AF, AFS, AFE, C, NewAF, afChoice.Constr ] =
       AdditionalFieldsBuilder[ T, R, RV, AF, AFS, AFE, C, NewAF, afChoice.Constr ]( this )

   def construct(
       using
       cType : ConstructorChooser[ T, RV, AF ],
   ) : ConstructorBuilder[ T, R, RV, AF, AFS, AFE, C, cType.Constr ] = {
       ConstructorBuilder[ T, R, RV, AF, AFS, AFE, C, cType.Constr ]( this )
   }

    def build(
       using
       uniq : => UniqueFieldNames[ R ],
       pc : ProductConstructor[ C, RV, AF, T ],
       afEv : ValidAfExtr[ T, AF, AFE ],
   ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] =
       ComplexSchema(
           ProductShape[ T, R, RV, AF, AFS, AFE, C ](
               fieldDescriptions = fieldDescs,
               additionalFieldsSchema = aftSch,
               afExtractor = afe,
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

case class ConstructorBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, NewC ](
    private[ product ] val bldr : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ]
)(
    using
    cType : ConstructorChooser.Aux[ T, RV, AF, NewC ],
    ctx : CtxWrapTuplesConstraint[ Field.Of, R, RV ],
) {
    def apply( constructor : NewC ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, NewC ] = {
        bldr.copy[ T, R, RV, AF, AFS, AFE, NewC ]( constr = constructor )
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
   fieldsConstraint : CtxWrapTuplesConstraint[ Field.Of, R, RV ],
   val afChoice : ConstrUpdateChoice.Aux[ RV, RV, AF, NewAF, C, NewC ],
) {
    def fromSchema[ S ](
        extract : T => Map[ String, NewAF ],
    )(
        implicit schema : Schema.Aux[ NewAF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, S, T => Map[ String, NewAF ], NewC ] = {
        ProductSchemaBuilder[ T, R, RV, NewAF, S, T => Map[ String, NewAF ], NewC ](
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
        extract: T => Map[ String, NewAF ],
    ) : ProductSchemaBuilder[ T, R, RV, NewAF, Unit, T => Map[ String, NewAF ], NewC ] = {
        fromSchema[ Unit ]( extract )( Primitive[ NewAF ]() )
    }
}

case class FieldUpdater[ F, N <: FieldName, S, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
    private[ product ] val field : Field[ T, F, N, S ],
    private[ product ] val bldr : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ]
) {
    def apply[ NewN <: FieldName, NewS, NewR <: Tuple ](
        builder: FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, T => F ] => Field[ T, F, NewN, NewS ],
    )(
        using
        fr : FieldReplacer.Aux[ N, R, T, F, NewN, NewS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] = {
        val startingBuilder = FieldBuilder.from( field )
        val newField = builder( startingBuilder )
        val newFieldDescriptions = fr.replace( bldr.fieldDescs, newField )

        new ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ](
            bldr.nm,
            bldr.desc,
            bldr.vals,
            bldr.exs,
            bldr.dep,
            bldr.aftSch,
            bldr.afe,
            newFieldDescriptions,
            bldr.constr,
        )
    }
}

case class FieldComponentUpdater[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, F, N <: FieldName, FS, Sel <: Tuple, Inner ](
    builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ],
) {
    def apply[ NewInner, NewN <: FieldName, NewFS, NewR <: Tuple ](
        updater : Inner => NewInner,
    )(
        using
        fu : FieldReplacer.Aux[ N, R, T, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
        cu : => ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], Sel, Inner, NewInner, ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] ],
    ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] = {
        cu.update( builder )( updater )
    }
}

case class ProductFieldBuilder[ T, F, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
    builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ],
) {
    def apply[ N <: FieldName, S ](
        build : FieldBuilder.Empty[ T, F ] => Field[ T, F, N, S ],
    )(
        using
        fc : => CtxWrapTuplesConstraint[ Field.Of, Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ] ],
        uniq : UniqueFieldNames[ Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ] ],
    ) : ProductSchemaBuilder[ T, Tuple.Concat[ R, Field[ T, F, N, S ] *: EmptyTuple ], Tuple.Concat[ RV, F *: EmptyTuple ], AF, AFS, AFE, Unit ] =
        builder.addField[ F, N, S ]( build( FieldBuilder[ T, F ] ) )
}

object ProductSchemaBuilder {
   def apply[ T ] : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ] =
     ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ](
       fieldDescs = EmptyTuple,
       aftSch = NoSchema,
       afe = (),
       constr = (),
     )

   def from[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
       schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
   )(
       using
       fieldsConstraint : CtxWrapTuplesConstraint[ Field.Of, R, RV ],
   ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] = {
       ProductSchemaBuilder(
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
