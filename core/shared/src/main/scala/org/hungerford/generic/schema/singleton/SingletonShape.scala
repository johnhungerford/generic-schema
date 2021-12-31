package org.hungerford.generic.schema.singleton

import org.hungerford.generic.schema.coproduct.subtype.TypeName

case class SingletonShape[ T <: Singleton, N <: TypeName ](
    name : N,
    value : T,
)
