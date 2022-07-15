package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.translation.RecursiveSchemaTranslator

trait UPickleSchemaTranslation
  extends UPickleProductTranslation
    with UPickleSingletonSchemaTranslation
    with UPickleCoproductSchemaTranslation {

    import upickle.default.*

    given [ T, S, Trans <: Tuple ](
        using
        encTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Writer ],
        decTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Reader ],
    ): RecursiveSchemaTranslator[ T, S, Trans, ReadWriter ] with {
        override def translate(
            schema: Aux[ T, S ],
            trans: Trans,
        ): ReadWriter[ T ] = {
            val encoder = encTr.translate( schema, EmptyTuple )
            val decoder = decTr.translate( schema, EmptyTuple )
            ReadWriter.join( decoder, encoder )
        }
    }

}

object UPickleSchemaTranslation
  extends UPickleSchemaTranslation
