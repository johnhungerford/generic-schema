package org.hungerford.generic.schema.circe

import io.circe.{Decoder, Encoder}

trait Codec[ T ] {
    def encoder : Encoder[ T ]
    def decoder : Decoder[ T ]
}
