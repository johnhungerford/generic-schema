package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.validator.Validator

trait Subtype[ T ] {
  type Name <: FieldName
  type Shape

  def typeName : Name
  def schema : Schema.Aux[ T, Shape ]
  def description : Option[ String ] = None
  def validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
  def default : Option[ T ] = None
  def examples : Seq[ T ] = Seq.empty[ T ]
  def deprecated : Boolean = false
}

case class SubtypeCase[ T, N <: FieldName, S ](
  typeName : N,
  schema : Schema.Aux[ T, S ],
  description : Option[ String ] = None,
  validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
  default : Option[ T ] = None,
  examples : Seq[ T ] = Seq.empty[ T ],
  deprecated : Boolean = false,
) {
  type Name = N
  type Shape = S
}
