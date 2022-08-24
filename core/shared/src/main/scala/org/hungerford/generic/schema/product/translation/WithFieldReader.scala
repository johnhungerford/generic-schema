package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.product.field.Field

trait WithFieldReader[ DecoderSchema[ _ ], Source ] {
    trait FieldReader[ F, N <: TypeName ] {
        def read(from : Source, subtype : Field.Tpe[ F ] & Field.Named[ N ], schema : DecoderSchema[ F ] ) : Option[ F ]
    }
}



