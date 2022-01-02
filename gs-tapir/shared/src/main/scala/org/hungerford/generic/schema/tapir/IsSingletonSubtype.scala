package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeCase, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.singleton.SingletonShape

trait IsSingletonSubtype[ T ]

object IsSingletonSubtype {
    given generic[ T, ST <: T & Singleton, D, DN, DV, N <: TypeName, STN <: TypeName, IsSubtype <: Subtype.Aux[ T, ST, D, DN, DV, N, SingletonShape[ ST, STN ] ] ] :
      IsSingletonSubtype[ IsSubtype ] with {}
}
