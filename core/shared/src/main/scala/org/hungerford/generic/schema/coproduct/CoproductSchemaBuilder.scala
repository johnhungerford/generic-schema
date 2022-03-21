package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{ComplexSchema, Schema}
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.{FieldName, FieldRetriever}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeBuilder, SubtypeRemover, SubtypeRetriever, SubtypeTypeRemover, ToSuperGenerator, TypeName}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Nat}
import org.hungerford.generic.schema.selector.{ComponentUpdater, SubTypeSelector, TypeSelector}

import scala.collection.immutable.ListMap


case class CoproductSchemaBuilder[ T, R <: Tuple, D, DN ](
    private[ schema ] val nm : Option[ String ] = None,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ schema ] val exs : Seq[ T ] = Nil,
    private[ schema ] val dep : Boolean = false,
    private[ schema ] val sts : R,
) {
    def name( name : String ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( nm = Some( name ) )
    def description( description : String ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        copy( vals = validators.toSet )
    def examples( example : T, otherExamples : T* ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        examples( example +: otherExamples )
    def examples( examples : Seq[ T ] ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( exs = examples.toSeq )
    def deprecate : CoproductSchemaBuilder[ T, R, D, DN ] = copy( dep = true )

    def discriminator[ NewD ](
        using
        da : DiscriminatorAdder[ NewD, T, R, D, DN ],
    ) : [ NewDN <: FieldName ] => NewDN => da.Out[ NewDN ] = da.add( nm, desc, vals, exs, dep, sts )

    def addSubtype[ ST, DV, N <: TypeName, SubT <: Subtype.OrLazy[ T, ST, D, DN, DV, N ] ](
        st : SubT,
    )(
        using
        uniqT : UniqueTypeNames[ Tuple.Concat[ R, SubT *: EmptyTuple ] ],
        uniqDV : UniqueDiscriminatorValues[ Tuple.Concat[ R, SubT *: EmptyTuple ] ],
        vd : ValidDiscriminator[ D, DN, Tuple.Concat[ R, SubT *: EmptyTuple ] ]
    ) : CoproductSchemaBuilder[ T, Tuple.Concat[ R, SubT *: EmptyTuple ], D, DN ] = {
        copy[ T, Tuple.Concat[ R, SubT *: EmptyTuple ], D, DN ](
            sts = sts ++ ( st *: EmptyTuple ),
        )
    }

    def buildSubtype[ ST ](
        using
        tsEv : ToSuperGenerator[ T, ST ],
    ) : SubtypeBuilderAdder[ ST, tsEv.TS, T, R, D, DN ] =
        SubtypeBuilderAdder[ ST, tsEv.TS, T, R, D, DN ]( this )

    def removeSubtype[ N <: Singleton, NewR <: Tuple ](
        typeName : N,
    )(
        using
        str : SubtypeRemover.Aux[ N, R, NewR ],
    ) : CoproductSchemaBuilder[ T, NewR, D, DN ] = {
        copy[ T, NewR, D, DN ]( sts = str.remove( sts ) )
    }

    def removeSubtype[ ST, N <: Nat, NewR <: Tuple ](
        typeSelector : TypeSelector[ ST, N ],
    )(
        using
        str : SubtypeTypeRemover.Aux[ ST, N, R, NewR ],
    ) : CoproductSchemaBuilder[ T, NewR, D, DN ] = {
        copy[ T, NewR, D, DN ]( sts = str.remove( sts ) )
    }

    def modifySubtype[ N <: TypeName, SubT ](
        subtype : N,
    )(
        using
        str : SubtypeRetriever.Aux[ N, R, SubT ],
    ) : SubtypeModifier[ N, R, CoproductSchemaBuilder[ T, R, D, DN ], SubT ] =
        SubtypeModifier[ N, R, CoproductSchemaBuilder[ T, R, D, DN ], SubT ]( this )

    def build[ RV <: Tuple ](
        using
        ctx : CtxWrapTuplesConstraint[ Subtype.Tpe, R, RV ],
        uniqT : UniqueTypeNames[ R ],
        uniqDV : UniqueDiscriminatorValues[ R ],
        dEv : ValidDiscriminator[ D, DN, R ],
    ) : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] = {
        val shape = CoproductShape[ T, R, RV, D, DN ]( sts )
        ComplexSchema[ T, CoproductShape[ T, R, RV, D, DN ] ]( shape, nm, desc, vals, exs, dep )
    }

}

object CoproductSchemaBuilder {
    def empty[ T ] = new CoproductSchemaBuilder[ T, EmptyTuple, Unit, Unit ](
        sts = EmptyTuple,
    )
}

trait DiscriminatorAdder[ NewD, T, R, D, DN ] {
    type Out[ _ ]

    def add(
        nm : Option[ String ] = None,
        desc : Option[ String ] = None,
        vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
        exs : Seq[ T ] = Nil,
        dep : Boolean = false,
        sts : R,
    ) : [ NewDN <: FieldName ] => NewDN => Out[ NewDN ]

}

object DiscriminatorAdder {
    given [ T, R <: Tuple, D, DN ] : DiscriminatorAdder[ D, T, R, D, DN ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, D, NewDN ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, D, NewDN ](
                nm, desc, vals, exs, dep, sts,
            )
        }
    }

    given [ NewD, T, R <: Tuple, D, DN ] : DiscriminatorAdder[ NewD, T, R, D, DN ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, NewD, NewDN ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, NewD, NewDN ](
                nm, desc, vals, exs, dep, sts,
            )
        }
    }
}

trait DiscrName[ DN ] {
    def value : DN
}

object DiscrName {
    given discrNameWithValue[ DN <: TypeName ](
        using
        dn : ValueOf[ DN ],
    ) : DiscrName[ DN ] with {
        override def value: DN = dn.value
    }

    given discrNameWithoutValue : DiscrName[ Unit ] with {
        override def value: Unit = ()
    }
}

case class SubtypeBuilderAdder[ ST, TSType, T, R <: Tuple, D, DN  ](
    stb : CoproductSchemaBuilder[ T, R, D, DN ],
)(
    using
    val tsEv: ToSuperGenerator.Aux[ T, ST, TSType ],
) {
    def apply[ DV, N <: TypeName, SubT <: Subtype.OrLazy[ T, ST, D, DN, DV, N ] ](
        buildFn: SubtypeBuilder[ T, ST, D, DN, Unit, TSType, Unit, Unit, Nothing, Unit ] => SubT,
    )(
        using
        dn : DiscrName[ DN ],
        uniqT : UniqueTypeNames[ Tuple.Concat[ R, SubT *: EmptyTuple ] ],
        uniqDV : UniqueDiscriminatorValues[ Tuple.Concat[ R, SubT *: EmptyTuple ] ],
        vd: ValidDiscriminator[ D, DN, Tuple.Concat[ R, SubT *: EmptyTuple ] ],
    ): CoproductSchemaBuilder[ T, Tuple.Concat[ R, SubT *: EmptyTuple ], D, DN ] =
        stb.addSubtype( buildFn( SubtypeBuilder.empty[ T, ST, D, DN ]( dn.value ) ) )
}

case class SubtypeModifier[ N <: TypeName, R <: Tuple, Builder, SubT ](
    builder : Builder
)(
    using
    str : SubtypeRetriever.Aux[ N, R, SubT ],
) {
    def apply[ NewSubT, NewBuilder ](
        modifier : SubT => NewSubT,
    )(
        using
        cm : ComponentUpdater.Aux[ Builder, SubTypeSelector[ N ], SubT, NewSubT, NewBuilder ]
    ) : NewBuilder = cm.update( builder )( modifier )
}
