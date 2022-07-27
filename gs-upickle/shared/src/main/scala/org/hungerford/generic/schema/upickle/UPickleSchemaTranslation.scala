package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.RecursiveSchemaTranslator
import org.hungerford.generic.schema.translation.TypeCache

trait UPickleSchemaTranslation
  extends UPickleProductTranslation
    with UPickleSingletonSchemaTranslation
    with UPickleCoproductSchemaTranslation {

    import upickle.default.*

    given [ T, S, Cache <: TypeCache ](
        using
        encTr: RecursiveSchemaTranslator[ T, S, TypeCache.Empty, Writer ],
        decTr: RecursiveSchemaTranslator[ T, S, TypeCache.Empty, Reader ],
    ): RecursiveSchemaTranslator[ T, S, Cache, ReadWriter ] with {
        override def translate(
            schema: Schema.Aux[ T, S ],
            cache: Cache,
        ): ReadWriter[ T ] = {
            val encoder = encTr.translate( schema, TypeCache.Empty )
            val decoder = decTr.translate( schema, TypeCache.Empty )
            ReadWriter.join( decoder, encoder )
        }
    }

}

object UPickleSchemaTranslation
  extends UPickleSchemaTranslation
