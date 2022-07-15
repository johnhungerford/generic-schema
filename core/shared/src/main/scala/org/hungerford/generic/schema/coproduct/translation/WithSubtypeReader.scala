package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}

trait WithSubtypeReader[ DecoderSchema[ _ ], Source ] {
    trait SubtypeReader[ ST, N <: TypeName ] {
        def read( from : Source, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : DecoderSchema[ ST ] ) : Option[ ST ]
    }

    trait DiscrReader[ DV ] {
        def read( from : Source, name : String, schema : DecoderSchema[ DV ] ) : Option[ DV ]
    }
}



