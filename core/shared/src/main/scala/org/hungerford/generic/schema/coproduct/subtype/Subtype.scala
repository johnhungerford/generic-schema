package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.{Schema, SchemaRebuilder}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

import scala.util.NotGiven

type TypeName = String & Singleton

sealed case class Subtype[ T, ST, D, DN, DV, N <: TypeName, S ] private[ schema ] (
    override val typeName: N,
    override val schema: Schema.Aux[ ST, S ],
    override val toSuper : ST => T,
    override val fromSuper : T => Option[ ST ],
    override val discriminatorValue : DV,
    override val description: Option[ String ] = None,
    override val validators: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ],
    override val default: Option[ ST ] = None,
    override val examples: Seq[ ST ] = Seq.empty[ ST ],
    override val deprecated: Boolean = false,
) extends Subtype.OrLazy[ T, ST, D, DN, DV, N ] with Subtype.Shaped[ ST, S ] {
    type Self = Subtype[ T, ST, D, DN, DV, N, S ]
}

sealed case class LazySubtype[ T, ST, D, DN, DV, N <: TypeName ] private[ schema ] (
    override val typeName: N,
    override val toSuper : ST => T,
    override val fromSuper : T => Option[ ST ],
    override val discriminatorValue : DV,
    override val description: Option[ String ] = None,
    override val validators: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ],
    override val default: Option[ ST ] = None,
    override val examples: Seq[ ST ] = Seq.empty[ ST ],
    override val deprecated: Boolean = false,
) extends Subtype.OrLazy[ T, ST, D, DN, DV, N ] {
    type Self = LazySubtype[ T, ST, D, DN, DV, N ]
}

object Subtype {
    sealed trait Subtype {
        def description: Option[ String ]
        def deprecated: Boolean
    }

    sealed trait Of[ T ] extends Subtype.Subtype

    sealed trait Tpe[ ST ] extends Subtype.Subtype {
        def validators: Set[ Validator[ ST ] ]
        def default: Option[ ST ]
        def examples: Seq[ ST ]
    }

    sealed trait Named[ N <: TypeName ] extends Subtype.Subtype {
        def typeName : N
    }

    sealed trait Discr[ D, DN, DV ] extends Subtype.Subtype {
        def discriminatorName : DN
        def discriminatorValue : DV
    }

    type NoDiscr = Discr[ Nothing, Nothing, Unit ]
    type WithDiscr[ D, DN <: TypeName, DV <: D & Singleton ] = Discr[ D, DN, DV ]

    sealed trait Shaped[ ST, Shape ] extends Subtype.Tpe[ ST ] {
        def schema : Schema.Aux[ ST, Shape ]
    }

    sealed trait SubOf[ T, ST ] extends Subtype.Of[ T ] with Subtype.Tpe[ ST ] {
        def toSuper : ST => T
        def fromSuper : T => Option[ ST ]
    }

    sealed trait OrLazy[ T, ST, D, DN, DV, N <: TypeName ] extends Subtype.SubOf[ T, ST ] with Subtype.Discr[ D, DN, DV ]
}


trait SubtypeDsl {

    extension ( subtype : Subtype.type )
        def builder[ T, ST, D, DN ](
            using
            tsEv : ToSuperGenerator[ T, ST ],
        ) : SubtypeBuilder[ T, ST, D, DN, Unit, tsEv.TS, Unit, Unit, Nothing, Unit ] =
            SubtypeBuilder.empty[ T, ST, D, DN ]

    extension [ T, ST, D, DN, DV, N <: TypeName, S ]( subtype : Subtype[ T, ST, D, DN, DV, N, S ] ) {
        def apply[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Subtype[ T, ST, D, DN, DV, N, S ], Sel, Inner ],
        ) : Inner = cr.retrieve( subtype )

        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Subtype[ T, ST, D, DN, DV, N, S ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ Subtype[ T, ST, D, DN, DV, N, S ], Inner, Sel ] =
            ComponentUpdater.Updater( subtype )

        def rebuild : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, T => Option[ ST ], N, S, Schema.Aux[ ST, S ] ] =
            SubtypeBuilder.from( subtype )

        def withName[ NewN <: TypeName ](
            name : NewN,
        ) : Subtype[ T, ST, D, DN, DV, NewN, S ] = subtype.copy( typeName = name )

        def withSchema[ NewS ](
            schema : Schema.Aux[ ST, NewS ],
        ) : Subtype[ T, ST, D, DN, DV, N, NewS ] = subtype.copy( schema = schema )

        def modifySchema[ NewS ](
            modifier : Schema.Aux[ ST, S ] => Schema.Aux[ ST, NewS ],
        ) : Subtype[ T, ST, D, DN, DV, N, NewS ] = subtype.copy( schema = modifier( subtype.schema ) )

        def withToSuper(
            toSuper : ST => T,
        ) : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( toSuper = toSuper )

        def withFromSuper(
            fromSuper : T => Option[ ST ],
        ) : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( fromSuper = fromSuper )

        def withDiscriminatorValue[ NewDV <: D & Singleton ](
            value : NewDV
        )(
            using
            ev : NotGiven[ D =:= Unit ],
        ) : Subtype[ T, ST, D, DN, NewDV, N, S ] = subtype.copy( discriminatorValue = value )

        def withoutDiscriminator : Subtype[ T, ST, Unit, Nothing, Unit, N, S ] =
            subtype.copy( discriminatorValue = () )

        def withDescription( description : String ) : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( description = Some( description ) )

        def withoutDescription : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( description = None )

        def withValidators(
            validator: Validator[ ST ],
            otherValidators : Validator[ ST ]*,
        ) : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( validators = ( validator +: otherValidators ).toSet )

        def addValidators(
            validator: Validator[ ST ],
            otherValidators : Validator[ ST ]*,
        ) : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( validators = subtype.validators ++ ( validator +: otherValidators ).toSet )

        def withoutValidators : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( validators = Set.empty )

        def asDeprecated : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( deprecated = true )

        def asUndeprecated : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( deprecated = false )

        def withExamples(
            example: ST,
            otherExamples : ST*,
        ) : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( examples = example +: otherExamples )

        def addExamples(
            example: ST,
            otherExamples : ST*,
        ) : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( examples = ( example +: otherExamples ) ++ subtype.examples )

        def withoutExamples : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( examples = Nil )

        def withDefault( default : ST )  : Subtype[ T, ST, D, DN, DV, N, S ] =
            subtype.copy( default = Some( default ) )

        def withoutDefault : Subtype[ T, ST, D, DN, DV, N, S ] = subtype.copy( default = None )

    }




}
