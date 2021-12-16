package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.validator.Validator

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
    type WithD[ T, ST, D, DN <: FieldName, DV <: D, N <: TypeName, S ] = Aux[ T, ST, D, DN, DV, N, S ]
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

}
