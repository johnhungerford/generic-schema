package org.hungerford.generic.schema.circe

import org.hungerford.generic.schema.translation.PrimitiveSchemaTranslatorTest

import io.circe.*

given [ T ](
    using
    enc : Encoder[ T ],
    dec : Decoder[ T ],
) : Codec[ T ] = Codec.from( dec, enc )

class CircePrimitiveSchemaTranslationTest extends PrimitiveSchemaTranslatorTest[ Codec ]
