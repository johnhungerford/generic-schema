package org.hungerford.generic.schema.product.translation

trait Encoder[ T, InformedBy, Encoded ] {
    def encode( value : T, informedBy : InformedBy ) : Encoded
}

trait Decoder[ T, InformedBy, Encoded ] {
    def decode( value : Encoded, informedBy : InformedBy ) : T
}
