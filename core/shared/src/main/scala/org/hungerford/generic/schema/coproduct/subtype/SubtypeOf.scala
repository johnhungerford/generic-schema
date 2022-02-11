package org.hungerford.generic.schema.coproduct.subtype

trait SubtypeOfName[ N <: TypeName, SubT ]

object SubtypeOfName {
    given [ N <: TypeName, T, ST, D, DN, DV, STS ] : SubtypeOfName[ N, Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ] with {}
}

trait SubtypeOfDV[ D, DV <: D & Singleton, SubT ]

object SubtypeOfDV {
    given [ N <: TypeName, T, ST, D, DN, DV <: D & Singleton, STS ] : SubtypeOfDV[ D, DV, Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ] with {}
}

trait SubtypeOfType[ T, SubT ]

object SubtypeOfType {
    given [ T, ST, D, DN, DV, N <: TypeName, STS ] : SubtypeOfType[ ST, Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ] with {}
}
