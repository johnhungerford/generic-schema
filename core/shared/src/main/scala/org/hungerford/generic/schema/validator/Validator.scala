package org.hungerford.generic.schema.validator

trait Validator[ T ] {
    def isValid( instance : T ) : Boolean
}
