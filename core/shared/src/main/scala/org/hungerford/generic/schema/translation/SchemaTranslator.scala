package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import shapeless.Lazy

import scala.language.higherKinds

trait SchemaTranslator[ T, OtherSchema[ _ ] ] {
    type OurSchema <: Schema[ T ]

    def translate( schema : OurSchema ) : OtherSchema[ T ]
}

object SchemaTranslator {
    type Aux[ T, OtherSchema[ _ ], OurSch <: Schema[ T ] ] = SchemaTranslator[ T, OtherSchema ] { type OurSchema = OurSch }

    def apply[ T, OtherSchema[ _ ], OurSch <: Schema[ T ] ](
        implicit sch : SchemaTranslator.Aux[ T, OtherSchema, OurSch ],
    ) : SchemaTranslator.Aux[ T, OtherSchema, OurSch ] = sch

    def translate[ T, OtherSchema[ _ ], OurSch <: Schema[ T ] ](
        sch : OurSch,
    )(
        implicit
        trans : SchemaTranslator.Aux[ T, OtherSchema, OurSch ],
    ) : OtherSchema[ T ] = trans.translate( sch )

}
