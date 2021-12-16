package org.hungerford.generic.schema

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeDsl}
import org.hungerford.generic.schema.selector.SelectorDsl
import org.hungerford.generic.schema.product.field.FieldDsl

class GenericSchemaDsl( conf : SchemaConfiguration )
    extends SchemaDsl
      with FieldDsl
      with SubtypeDsl
      with SelectorDsl {

    given SchemaConfiguration = conf

}
