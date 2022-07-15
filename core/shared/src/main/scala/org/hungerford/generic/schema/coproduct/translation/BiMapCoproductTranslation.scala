package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}

trait BiMapCoproductTranslation[ RW[ _ ], DecoderSch[ _ ], EncoderSch[ _ ], Source, Sink ]
    extends CoproductDecoderTranslation[ DecoderSch, Source ]
      with CoproductEncoderTranslation[ EncoderSch, Sink ] {

    def buildCoproductSchema[ T ]( enc : EncoderSch[ T ], dec : DecoderSch[ T ] ) : RW[ T ]

}
