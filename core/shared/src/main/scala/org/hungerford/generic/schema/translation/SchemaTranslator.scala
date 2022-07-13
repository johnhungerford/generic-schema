package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux

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
        using
        os : OtherSchema[ T ],
    ) : SchemaTranslator[ T, Unit, OtherSchema ] =
        ( _ : Schema.Aux[ T, Unit ] ) => os

    given fromRecursive[ T, S, OtherSchema[ _ ] ](
        using
        tr : RecursiveSchemaTranslator[ T, S, EmptyTuple, OtherSchema ],
    ): SchemaTranslator[ T, S, OtherSchema ] with {
        override def translate(
            schema: Aux[ T, S ]
        ): OtherSchema[ T ] = tr.translate( schema, EmptyTuple )
    }

    def translate[ T, S,  OtherSchema[ _ ] ](
        sch : Schema.Aux[ T, S ],
    )(
        using
        trans : SchemaTranslator[ T, S, OtherSchema ],
    ) : OtherSchema[ T ] = trans.translate( sch )

}

trait RecursiveSchemaTranslator[ T, S, Trans <: Tuple, OtherSchema[ _ ] ] {
    def translate( schema : Schema.Aux[ T, S ], translators: Trans ) : OtherSchema[ T ]
}

object RecursiveSchemaTranslator {
    def apply[ T, S, Trans <: Tuple, OtherSchema[ _ ] ](
        implicit sch : RecursiveSchemaTranslator[ T, S, Trans, OtherSchema ],
    ) : RecursiveSchemaTranslator[ T, S, Trans, OtherSchema ] = sch

    /**
     * Resolves type class instances for primitive schemas
     */
    implicit def primitiveTranslation[ T, Trans <: Tuple, OtherSchema[ _ ] ](
        using
        os : OtherSchema[ T ],
    ) : RecursiveSchemaTranslator[ T, Unit, Trans, OtherSchema ] =
        ( _ : Schema.Aux[ T, Unit ], _ : Trans ) => os

    def translate[ T, S, Trans <: Tuple, OtherSchema[ _ ] ](
        sch : Schema.Aux[ T, S ],
        trans : Trans,
    )(
        using
        recTrans : RecursiveSchemaTranslator[ T, S, Trans, OtherSchema ],
    ) : OtherSchema[ T ] = recTrans.translate( sch, trans )

}
