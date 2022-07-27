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
        tr : RecursiveSchemaTranslator[ T, S, TypeCache.Empty, OtherSchema ],
    ): SchemaTranslator[ T, S, OtherSchema ] with {
        override def translate(
            schema: Aux[ T, S ]
        ): OtherSchema[ T ] = tr.translate( schema, TypeCache.Empty )
    }

    def translate[ T, S,  OtherSchema[ _ ] ](
        sch : Schema.Aux[ T, S ],
    )(
        using
        trans : SchemaTranslator[ T, S, OtherSchema ],
    ) : OtherSchema[ T ] = trans.translate( sch )

}

trait RecursiveSchemaTranslator[ T, S, Cache <: TypeCache, OtherSchema[ _ ] ] {
    def translate( schema : Schema.Aux[ T, S ], cache: Cache ) : OtherSchema[ T ]
}

object RecursiveSchemaTranslator {
    def apply[ T, S, Cache <: TypeCache, OtherSchema[ _ ] ](
        implicit sch : RecursiveSchemaTranslator[ T, S, Cache, OtherSchema ],
    ) : RecursiveSchemaTranslator[ T, S, Cache, OtherSchema ] = sch

    /**
     * Resolves type class instances for primitive schemas
     */
    implicit def primitiveTranslation[ T, Cache <: TypeCache, OtherSchema[ _ ] ](
        using
        os : OtherSchema[ T ],
    ) : RecursiveSchemaTranslator[ T, Unit, Cache, OtherSchema ] =
        ( _ : Schema.Aux[ T, Unit ], _ : Cache ) => os

    def translate[ T, S, Cache <: TypeCache, OtherSchema[ _ ] ](
        sch : Schema.Aux[ T, S ],
        trans : Cache,
    )(
        using
        recTrans : RecursiveSchemaTranslator[ T, S, Cache, OtherSchema ],
    ) : OtherSchema[ T ] = recTrans.translate( sch, trans )

}
