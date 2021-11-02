package org.hungerford.generic.schema

import shapeless._

trait SchemaDeriver[ T, S <: Schema[ T ] ] {
    def derive : S
}

object SchemaDeriver {

    def apply[ T, S <: Schema[ T ] ](
        implicit sd : SchemaDeriver[ T, S ],
    ) : SchemaDeriver[ T, S ] = sd

    implicit def productDeriver[ T, R <: HList, S <: Schema[ T ] ](
        implicit
        gen : LabelledGeneric.Aux[ T, R ],
        prodDeriv : ProductDeriver.Aux[ R, S ],
    ) : SchemaDeriver[ T, S ]

}
