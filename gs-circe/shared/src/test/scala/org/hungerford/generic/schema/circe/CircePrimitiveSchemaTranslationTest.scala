package org.hungerford.generic.schema.circe
//
//import org.hungerford.generic.schema.translation.PrimitiveSchemaTranslatorTest
//
import io.circe._

given [ T ](
    using
    enc : Encoder[ T ],
    dec : Decoder[ T ],
) : Codec[ T ] = Codec.from( dec, enc )
//
//given enc[T](using cd: Codec[T]): Encoder[T] = Encoder.instance((t: T) => cd.apply(t))
//given dec[T](using cd: Codec[T]): Decoder[T] = Decoder.instance((hc: HCursor) => cd.apply(hc))
//
//
//class CircePrimitiveSchemaTranslationTest
//  extends PrimitiveSchemaTranslatorTest[ Codec ]
