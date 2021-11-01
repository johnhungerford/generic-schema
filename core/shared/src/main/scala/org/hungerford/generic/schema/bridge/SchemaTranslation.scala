package org.hungerford.generic.schema.bridge

import scala.language.higherKinds

trait SchemaTranslation[ T, OurSchema[ _ ], OtherSchema[ _ ] ] {
    def translate( schema : OurSchema[ T ] ) : OtherSchema[ T ]
}
