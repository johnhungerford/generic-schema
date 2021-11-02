package org.hungerford.generic.schema.bridge

import org.hungerford.generic.schema.Schema

import scala.language.higherKinds

trait SchemaTranslation[ T, OurSchema <: Schema[ T ], OtherSchema[ _ ] ] {
    def translate( schema : OurSchema ) : OtherSchema[ T ]
}
