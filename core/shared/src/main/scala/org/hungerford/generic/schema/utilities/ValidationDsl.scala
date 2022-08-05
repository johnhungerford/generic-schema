package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.types.Validation

trait ValidationDsl {
    extension [T](value: T)
        def isValid[S](
            using
            sch: Schema.Aux[T, S],
            vld: Validation[T, Schema.Aux[T, S]],
        ): Boolean = vld.isValid(value, sch)
}

object ValidationDsl extends ValidationDsl
