package org.hungerford.generic.schema.validator

trait Validator[ T ] {
    def isValid( instance : T ) : Boolean
}

object Validator {
    def min[ T : Ordering ]( minValue : T ) = new Min[ T ]( minValue )
    def exclusiveMin[ T : Ordering ]( maxValue : T ) = new Max[ T ]( maxValue, true )
    def max[ T : Ordering ]( maxValue : T ) = new Max[ T ]( maxValue )
    def exclusiveMax[ T : Ordering ]( maxValue : T ) = new Max[ T ]( maxValue, true )
}

class Min[ T : Ordering ]( minValue : T, exclusive : Boolean = false )
  extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = {
        val ord = summon[ Ordering[ T ] ]
        if ( exclusive ) ord.gt( instance, minValue )
        else ord.gteq( instance, minValue )
    }
}

class Max[ T : Ordering ]( maxValue : T, exclusive : Boolean = false )
  extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = {
        val ord = summon[ Ordering[ T ] ]
        if ( exclusive ) ord.lt( instance, maxValue )
        else ord.lteq( instance, maxValue )
    }
}
