package org.hungerford.generic.schema

import org.hungerford.generic.schema.selector.SelectorDsl

class GenericSchemaDsl( conf : SchemaConfiguration )
    extends SelectorDsl {

    given SchemaConfiguration = conf

}
