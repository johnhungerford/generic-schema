package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}

trait WithSubtypeWriter[ EncoderSchema[ _ ], Sink ] {
    trait SubtypeWriter[ ST, N <: TypeName ] {
        def write( value : ST, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : EncoderSchema[ ST ] ) : Sink
    }
}
