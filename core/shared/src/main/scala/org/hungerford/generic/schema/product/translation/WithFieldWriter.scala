package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.product.field.Field

trait WithFieldWriter[ EncoderSchema[ _ ], Sink ] {
    trait FieldWriter[ F, N <: TypeName ] {
        def write( value : F, subtype : Field.Of[ F ] & Field.Named[ N ], schema : EncoderSchema[ F ] ) : Sink
    }
}
