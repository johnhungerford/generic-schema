package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.validator.Validator

type TypeName = String & Singleton

trait Subtype[ T ] {
    type Name <: TypeName
    type Shape

    def typeName: Name

    def schema: Schema.Aux[ T, Shape ]

    def description: Option[ String ] = None

    def validators: Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]

    def default: Option[ T ] = None

    def examples: Seq[ T ] = Seq.empty[ T ]

    def deprecated: Boolean = false
}

object Subtype {
    type Aux[ T, N <: TypeName, S ] = Subtype[ T ] { type Name = N; type Shape = S }
}

case class SubtypeCase[ T, N <: TypeName, S ](
    override val typeName: N,
    override val schema: Schema.Aux[ T, S ],
    override val description: Option[ String ] = None,
    override val validators: Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    override val default: Option[ T ] = None,
    override val examples: Seq[ T ] = Seq.empty[ T ],
    override val deprecated: Boolean = false,
) extends Subtype[ T ] {
    type Name = N
    type Shape = S

    // For testing: get aux type from an instance
    type ST = Subtype.Aux[ T, N, S ]
}
