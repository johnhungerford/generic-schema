package org.hungerford.generic.schema.validator

import scala.util.matching.Regex

trait Validator[ T ] {
    def isValid( instance : T ) : Boolean
}

object Validator {
    // Ordering validators
    def min[ T : Ordering ]( minValue : T ) = new Min[ T ]( minValue )
    def minExclusive[ T : Ordering ]( minValue : T ) = new Min[ T ]( minValue, true )
    def max[ T : Ordering ]( maxValue : T ) = new Max[ T ]( maxValue )
    def maxExclusive[ T : Ordering ](maxValue : T ) = new Max[ T ]( maxValue, true )

    // String validators
    def regex( pattern : Regex ) : Validator[ String ] = new Regx( pattern )
    def regex( pattern : String ) : Validator[ String ] = new Regx( pattern.r )
    def minLength( length : Int ) = new StringLength( min[ Int ]( length ) )
    def minLengthExclusive( length : Int ) = new StringLength( minExclusive[ Int ]( length ) )
    def maxLength( length : Int ) = new StringLength( max[ Int ]( length ) )
    def maxLengthExclusive( length : Int ) = new StringLength( maxExclusive[ Int ]( length ) )
    def length( length : Int ) = new StringLength( ( instance: Int ) => instance == length )
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

class Regx( pattern : Regex ) extends Validator[ String ] {
    override def isValid( instance: String ): Boolean = pattern.matches( instance )
}

class StringLength( lengthValidator : Validator[ Int ] ) extends Validator[ String ] {
    override def isValid( instance: String ): Boolean = lengthValidator.isValid( instance.length )
}
