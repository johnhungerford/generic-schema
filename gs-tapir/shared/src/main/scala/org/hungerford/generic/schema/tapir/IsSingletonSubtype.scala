package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.singleton.SingletonShape

trait IsSingletonSubtype[ T ]

object IsSingletonSubtype {
    given generic[ T, ST <: T & Singleton, D, DN, DV, N <: TypeName, STN <: TypeName, IsSubtype <: Subtype[ T, ST, D, DN, DV, N, SingletonShape[ ST, STN ] ] ] :
      IsSingletonSubtype[ IsSubtype ] with {}
    
    given lazySt[ T, ST <: T & Singleton, D, DN, DV, N <: TypeName, STN <: TypeName, IsSubtype <: LazySubtype[ T, ST, D, DN, DV, N ] ](
        using
        singletonSchema : Schema.Aux[ ST, SingletonShape[ ST, STN ] ],
    ) : IsSingletonSubtype[ IsSubtype ] with {}
}
