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
    def minLength( length : Int ) : Validator[ String ] = StringLength( min[ Int ]( length ) )
    def minLengthExclusive( length : Int ) : Validator[ String ] = StringLength( minExclusive[ Int ]( length ) )
    def maxLength( length : Int ) : Validator[ String ] = StringLength( max[ Int ]( length ) )
    def maxLengthExclusive( length : Int ) : Validator[ String ] = StringLength( maxExclusive[ Int ]( length ) )
    def length( length : Int ) : Validator[ String ] = StringLength( ( instance: Int ) => instance == length )
    def nonEmptyString : Validator[ String ] = StringLength( min[ Int ]( 1 ) )

    // Collections validators
    def minSize[ T ]( length : Int ) : Validator[ Iterable[ T ] ] = CollSize[ T ]( min[ Int ]( length ) )
    def minSizeExclusive[ T ]( length : Int ) : Validator[ Iterable[ T ] ] =
        CollSize[ T ]( minExclusive[ Int ]( length ) )
    def maxSize[ T ]( length : Int ) : Validator[ Iterable[ T ] ] = CollSize[ T ]( max[ Int ]( length ) )
    def maxSizeExclusive[ T ]( length : Int ) : Validator[ Iterable[ T ] ] =
        CollSize[ T ]( maxExclusive[ Int ]( length ) )
    def size[ T ]( length : Int ) : Validator[ Iterable[ T ] ] =
        CollSize[ T ]( ( instance: Int ) => instance == length )
    def nonEmptyCollection[ T ] : Validator[ Iterable[ T ] ] = CollSize[ T ]( min[ Int ]( 1 ) )

    // Enum validator
    def oneOf[ T ]( possibleValues : Iterable[ T ] ) : Validator[ T ] = new OneOf[ T ]( possibleValues.toSet )
    def oneOf[ T ]( value : T, otherValues : T* ) : Validator[ T ] = oneOf[ T ]( value +: otherValues )
}

case class Min[ T : Ordering ]( minValue : T, exclusive : Boolean = false )
  extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = {
        val ord = summon[ Ordering[ T ] ]
        if ( exclusive ) ord.gt( instance, minValue )
        else ord.gteq( instance, minValue )
    }
}

case class Max[ T : Ordering ]( maxValue : T, exclusive : Boolean = false )
  extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = {
        val ord = summon[ Ordering[ T ] ]
        if ( exclusive ) ord.lt( instance, maxValue )
        else ord.lteq( instance, maxValue )
    }
}

case class Regx( pattern : Regex ) extends Validator[ String ] {
    override def isValid( instance: String ): Boolean = pattern.matches( instance )
}

case class StringLength( lengthValidator : Validator[ Int ] ) extends Validator[ String ] {
    override def isValid( instance: String ): Boolean = lengthValidator.isValid( instance.length )
}

case class OneOf[ T ]( possibleValues : Set[ T ] ) extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = possibleValues.contains( instance )
}

case class CollSize[ T ]( sizeValidator : Validator[ Int ] ) extends Validator[ Iterable[ T ] ] {
    override def isValid( instance: Iterable[ T ] ): Boolean = sizeValidator.isValid( instance.size )
}
