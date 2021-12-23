package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.{Schema, SchemaRebuilder}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

import scala.util.NotGiven

type TypeName = String & Singleton

trait Subtype[ T, ST, Discr ] {
    type DiscrName
    type DiscrValue
    type Name <: TypeName
    type Shape

    def typeName: Name

    def schema: Schema.Aux[ ST, Shape ]

    def asSuper : ST => T

    def discriminatorValue : DiscrValue

    def description: Option[ String ] = None

    def validators: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ]

    def default: Option[ ST ] = None

    def examples: Seq[ ST ] = Seq.empty[ ST ]

    def deprecated: Boolean = false
}

object Subtype {
    type Aux[ T, ST, D, DN, DV, N <: TypeName, S ] = Subtype[ T, ST, D ] {
        type Name = N; type DiscrValue = DV; type DiscrName = DN; type Shape = S
    }
    type Ctx[ T, D ] = [ X ] =>> Subtype[ T, X, D ]

    type NoD[ T, ST, N <: TypeName, S ] = Aux[ T, ST, Unit, Nothing, Nothing, N, S ]
    type WithD[ T, ST, D, DN <: FieldName, DV <: D & Singleton, N <: TypeName, S ] = Aux[ T, ST, D, DN, DV, N, S ]
}

case class SubtypeCase[ T, ST, D, DN, DV, N <: TypeName, S ](
    override val typeName: N,
    override val schema: Schema.Aux[ ST, S ],
    override val asSuper : ST => T,
    override val discriminatorValue : DV,
    override val description: Option[ String ] = None,
    override val validators: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ],
    override val default: Option[ ST ] = None,
    override val examples: Seq[ ST ] = Seq.empty[ ST ],
    override val deprecated: Boolean = false,
) extends Subtype[ T, ST, D ] {
    type Name = N
    type DiscrName = DN
    type DiscrValue = DV
    type Shape = S

    // For testing: get aux type from an instance
    type Aux = Subtype.Aux[ T, ST, D, DN, DV, N, S ]
}

trait SubtypeDsl {

    extension ( subtype : Subtype.type )
        def builder[ T, ST, D, DN ](
            using
            asEv : AsSuperGenerator[ T, ST ],
        ) : SubtypeBuilder[ T, ST, D, DN, Unit, asEv.AS, Unit, Nothing, Unit ] =
            SubtypeBuilder.empty[ T, ST, D, DN ]

    extension [ T, ST, D, DN, DV, N <: TypeName, S ]( subtype : Subtype.Aux[ T, ST, D, DN, DV, N, S ] ) {
        def apply[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Subtype.Aux[ T, ST, D, DN, DV, N, S ], Sel, Inner ],
        ) : Inner = cr.retrieve( subtype )

        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Subtype.Aux[ T, ST, D, DN, DV, N, S ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ Subtype.Aux[ T, ST, D, DN, DV, N, S ], Inner, Sel ] =
            ComponentUpdater.Updater( subtype )

        def rebuild : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, N, S, Schema.Aux[ ST, S ] ] =
            SubtypeBuilder.from( subtype )

        def withName[ NewN <: TypeName ](
            name : NewN,
        ) : Subtype.Aux[ T, ST, D, DN, DV, NewN, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( typeName = name )
        }

        def withSchema[ NewS ](
            schema : Schema.Aux[ ST, NewS ],
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, NewS ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( schema = schema )
        }

        def modifySchema[ NewS ](
            modifier : Schema.Aux[ ST, S ] => Schema.Aux[ ST, NewS ],
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, NewS ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( schema = modifier( subtype.schema ) )
        }

        def withAsSuper(
            asSuper : ST => T,
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( asSuper = asSuper )
        }

        def withDiscriminatorValue[ NewDV <: D & Singleton ](
            value : NewDV
        )(
            using
            ev : NotGiven[ D =:= Unit ],
        ) : Subtype.Aux[ T, ST, D, DN, NewDV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( discriminatorValue = value )
        }

        def withoutDiscriminator : Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( discriminatorValue = () )
        }

        def withDescription( description : String ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( description = Some( description ) )
        }

        def withoutDescription : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked => sc.copy( description = None )
        }

        def withValidators(
            validator: Validator[ ST ],
            otherValidators : Validator[ ST ]*,
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( validators = ( validator +: otherValidators ).toSet )
        }

        def addValidators(
            validator: Validator[ ST ],
            otherValidators : Validator[ ST ]*,
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( validators = subtype.validators ++ ( validator +: otherValidators ).toSet )
        }

        def withoutValidators : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( validators = Set.empty )
        }

        def asDeprecated : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( deprecated = true )
        }

        def asUndeprecated : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( deprecated = false )
        }

        def withExamples(
            example: ST,
            otherExamples : ST*,
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( examples = example +: otherExamples )
        }

        def addExamples(
            example: ST,
            otherExamples : ST*,
        ) : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( examples = ( example +: otherExamples ) ++ subtype.examples )
        }

        def withoutExamples : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( examples = Nil )
        }

        def withDefault( default : ST )  : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( default = Some( default ) )
        }

        def withoutDefault : Subtype.Aux[ T, ST, D, DN, DV, N, S ] = subtype match {
            case sc : SubtypeCase[ T, ST, D, DN, DV, N, S ] @unchecked =>
                sc.copy( default = None )
        }

    }




}
