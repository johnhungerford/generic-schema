package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.validator.Validator

case class ProductSchemaBuilder[ T ](
    private val desc : Option[ String ] = None,
    private val vals : Set[ Validator[ T ] ] = Set.empty,
)
