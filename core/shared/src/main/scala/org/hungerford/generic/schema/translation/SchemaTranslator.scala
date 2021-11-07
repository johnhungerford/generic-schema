package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import shapeless.Lazy

import scala.language.higherKinds

trait SchemaTranslator[ T, S, OtherSchema[ _ ] ] {
    def translate( schema : Schema.Aux[ T, S ] ) : OtherSchema[ T ]
}

object SchemaTranslator {
    def apply[ T, S, OtherSchema[ _ ] ](
        implicit sch : SchemaTranslator[ T, S, OtherSchema ],
    ) : SchemaTranslator[ T, S, OtherSchema ] = sch

    /**
     * Resolves type class instances for primitive schemas
     */
    implicit def primitiveTranslation[ T, OtherSchema[ _ ] ](
        implicit os : OtherSchema[ T ],
    ) : SchemaTranslator[ T, Unit, OtherSchema ] =
        ( _ : Schema.Aux[ T, Unit ] ) => os

    def translate[ T, S,  OtherSchema[ _ ] ](
        sch : Schema.Aux[ T, S ],
    )(
        implicit
        trans : SchemaTranslator[ T, S, OtherSchema ],
    ) : OtherSchema[ T ] = trans.translate( sch )

}
