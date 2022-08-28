package org.hungerford.generic.schema.validator

import scala.util.matching.Regex

trait Validator[ T ] {
    def isValid( instance : T ) : Boolean
}

object Validator {
    def apply[ T ]( fn : T => Boolean ) : Validator[ T ] = ( instance: T ) => fn( instance )

    // Ordering validators
    def min[ T : Ordering ]( minValue : T ) : Min[ T ] = Min[ T ]( minValue )
    def minExclusive[ T : Ordering ]( minValue : T ) : Min[ T ] = new Min[ T ]( minValue, true )
    def max[ T : Ordering ]( maxValue : T ) : Max[ T ] = new Max[ T ]( maxValue )
    def maxExclusive[ T : Ordering ](maxValue : T ) : Max[ T ] = new Max[ T ]( maxValue, true )

    // Numeric validators
    def positiveOrZero[ T : Numeric ] : PositiveOrZero[ T ] = PositiveOrZero[ T ]()
    def negativeOrZero[ T : Numeric ] : NegativeOrZero[ T ] = NegativeOrZero[ T ]()
    def nonZero[ T : Numeric ] : NonZero[ T ] = NonZero[ T ]()

    // String validators
    def regex( pattern : Regex ) : Validator[ String ] = new Regx( pattern )
    def regex( pattern : String ) : Validator[ String ] = new Regx( pattern.r )
    def minLength( length : Int ) : Validator[ String ] = StringLength( min[ Int ]( length ) )
    def minLengthExclusive( length : Int ) : Validator[ String ] = StringLength( minExclusive[ Int ]( length ) )
    def maxLength( length : Int ) : Validator[ String ] = StringLength( max[ Int ]( length ) )
    def maxLengthExclusive( length : Int ) : Validator[ String ] = StringLength( maxExclusive[ Int ]( length ) )
    def fixedLength(length : Int ) : Validator[ String ] = StringLength( EqValidator( length ) )
    def nonEmptyString : Validator[ String ] = StringLength( min[ Int ]( 1 ) )

    // Collections validators
    def minSize[ Col : Collection ]( length : Int ) : Validator[ Col ] =
        CollSize[ Col ]( min[ Int ]( length ) )
    def minSizeExclusive[ Col : Collection ]( length : Int ) : Validator[ Col ] =
        CollSize[ Col ]( minExclusive[ Int ]( length ) )
    def maxSize[ Col : Collection ]( length : Int ) : Validator[ Col ] =
        CollSize[ Col]( max[ Int ]( length ) )
    def maxSizeExclusive[ Col : Collection ]( length : Int ) : Validator[ Col ] =
        CollSize[ Col ]( maxExclusive[ Int ]( length ) )
    def fixedSize[ Col : Collection ]( length : Int ) : Validator[ Col ] =
        CollSize[ Col ]( EqValidator( length ) )
    def nonEmptyCollection[ Col : Collection ] : Validator[ Col ] =
        CollSize[ Col ]( min[ Int ]( 1 ) )

    // Enum validators
    def oneOf[ T ]( possibleValues : Iterable[ T ] ) : Validator[ T ] = new OneOf[ T ]( possibleValues.toSet )
    def oneOf[ T ]( value : T, otherValues : T* ) : Validator[ T ] = oneOf[ T ]( value +: otherValues )
    def noneOf[ T ]( excludedValues : Iterable[ T ] ) : Validator[ T ] = new NoneOf[ T ]( excludedValues.toSet )
    def noneOf[ T ]( value : T, otherValues : T* ) : Validator[ T ] = noneOf[ T ]( value +: otherValues )
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

case class EqValidator[ T ]( mustEqual : T ) extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = instance == mustEqual
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

case class NoneOf[ T ]( excludedValues : Set[ T ] ) extends Validator[ T ] {
    override def isValid( instance: T ): Boolean = !excludedValues.contains( instance )
}

case class CollSize[ Col : Collection ]( sizeValidator : Validator[ Int ] ) extends Validator[ Col ] {
    override def isValid( instance: Col ): Boolean = sizeValidator.isValid( Collection[Col].iterable(instance).size )
}

case class NonZero[ T : Numeric ]() extends Validator[ T ] {
    private val num : Numeric[ T ] = implicitly
    override def isValid( instance: T ): Boolean = instance != num.zero
}

case class PositiveOrZero[ T : Numeric ]() extends Validator[ T ] {
    private val num : Numeric[ T ] = implicitly
    override def isValid( instance: T ): Boolean = num.gteq( instance, num.zero )
}

case class NegativeOrZero[ T : Numeric ]() extends Validator[ T ] {
    private val num : Numeric[ T ] = implicitly
    override def isValid( instance: T ): Boolean = num.lteq( instance, num.zero )
}
