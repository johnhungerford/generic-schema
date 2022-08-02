package generic.schema

import org.hungerford.generic.schema.coproduct.subtype.{TypeName, SubtypeDsl}
import org.hungerford.generic.schema.defaults.DefaultSchemas
import org.hungerford.generic.schema.product.field.{FieldName, FieldDsl}
import org.hungerford.generic.schema.selector.SelectorDsl
import org.hungerford.generic.schema.{SchemaDsl, UtilitiesDsl}

object exports
  extends FieldDsl
    with SubtypeDsl
    with SchemaDsl
    with SelectorDsl {

    val Schema = org.hungerford.generic.schema.Schema
    type Schema[ T ] = org.hungerford.generic.schema.Schema[ T ]

    val Field = org.hungerford.generic.schema.product.field.Field
    type Field[ T, F, N <: FieldName, S ] = org.hungerford.generic.schema.product.field.Field[ T, F, N, S ]

    val Subtype = org.hungerford.generic.schema.coproduct.subtype.Subtype
    type Subtype[ T, ST, D, DN, DV, N <: TypeName, S ] = org.hungerford.generic.schema.coproduct.subtype.Subtype[ T,
      ST, D, DN, DV, N, S ]

    val Validator = org.hungerford.generic.schema.validator.Validator
    type Validator[ T ] = org.hungerford.generic.schema.validator.Validator[ T ]

}

object defaults extends DefaultSchemas

object utilities extends UtilitiesDsl
