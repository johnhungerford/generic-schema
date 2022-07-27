package org.hungerford.generic.schema.tapir

import sttp.tapir.{Validator as TapirValidator, ValidationError, ValidationResult}
import org.hungerford.generic.schema.validator.*

import scala.util.matching.Regex

trait TapirValidatorTranslation[ T ] {
  def translate( validator : Validator[ T ] ) : TapirValidator[ T ]
}

trait LowPriorityValidatorTranslations {
  def unboundedTranslation[ T ] : PartialFunction[ Validator[ T ], TapirValidator[ T ] ] = {
    case OneOf( possibleValues ) => TapirValidator.enumeration( possibleValues.toList )
    case NoneOf( excludedValues ) => TapirValidator.custom( t => {
      if ( excludedValues.contains( t ) ) ValidationResult.Invalid( List( "value is excluded" ) )
      else ValidationResult.Valid
    } )
  }

  given unboundedTrans[ T ] : TapirValidatorTranslation[ T ] with {
    override def translate(validator: Validator[T]): TapirValidator[T] = unboundedTranslation[ T ]( validator )
  }
}

object TapirValidatorTranslation extends LowPriorityValidatorTranslations {
  given stringTrans : TapirValidatorTranslation[ String ] with {
    override def translate( validator: Validator[ String ] ): TapirValidator[ String ] =
      unboundedTranslation.applyOrElse( validator, {
        case StringLength(Min(minValue, false)) => TapirValidator.minLength(minValue)
        case StringLength(Min(minValue, true)) => TapirValidator.minLength(minValue + 1)
        case StringLength(Max(maxValue, false)) => TapirValidator.maxLength(maxValue)
        case StringLength(Max(maxValue, true)) => TapirValidator.maxLength(maxValue - 1)
        case StringLength(EqValidator(length)) => TapirValidator.fixedLength(length)
        case Regx(pattern: Regex) => TapirValidator.pattern(pattern.regex)
        case tr => TapirValidator.custom( t => {
          if ( tr.isValid( t ) ) ValidationResult.Valid
          else ValidationResult.Invalid( List( "invalid" ) )
        } )
      } )
  }

  given iterableTrans[ Col[ _ ] <: Iterable[ _ ], T ] : TapirValidatorTranslation[ Col[ T ] ] with {
    override def translate( validator: Validator[ Col[ T ] ] ): TapirValidator[ Col[ T ] ] =
      unboundedTranslation.applyOrElse( validator, {
        case CollSize(Min(minValue, false)) => TapirValidator.minSize(minValue)
        case CollSize(Min(minValue, true)) => TapirValidator.minSize(minValue + 1)
        case CollSize(Max(maxValue, false)) => TapirValidator.maxSize(maxValue)
        case CollSize(Max(maxValue, true)) => TapirValidator.maxSize(maxValue - 1)
        case CollSize(EqValidator(length)) => TapirValidator.fixedSize(length)
        case tr => TapirValidator.custom( t => {
          if ( tr.isValid( t ) ) ValidationResult.Valid
          else ValidationResult.Invalid( List( "invalid" ) )
        } )
      } )
  }

  given numericTrans[ T : Numeric ] : TapirValidatorTranslation[ T ] with {
    override def translate(validator: Validator[T]): TapirValidator[T] =
      unboundedTranslation.applyOrElse(validator, {
        case PositiveOrZero() => TapirValidator.positiveOrZero
        case NegativeOrZero() => TapirValidator.custom( t => {
          val num = implicitly[ Numeric[ T ] ]
          if ( num.gt( t, num.zero ) ) ValidationResult.Invalid( List( "value was not negative or zero" ) )
          else ValidationResult.Valid
        } )
        case NonZero() => TapirValidator.custom( t => {
          val num = implicitly[ Numeric[ T ] ]
          if ( t == num.zero ) ValidationResult.Invalid( List( "value was equal to zero" ) )
          else ValidationResult.Valid
        } )
        case Min( minValue, exclusive ) => TapirValidator.Min( minValue, exclusive )
        case Max( maxValue, exclusive ) => TapirValidator.Max( maxValue, exclusive )
        case tr => TapirValidator.custom( t => {
          if ( tr.isValid( t ) ) ValidationResult.Valid
          else ValidationResult.Invalid( List( "invalid" ) )
        } )
      } )
  }

  def translate[ T ]( validator : Validator[ T ] )( using tr : TapirValidatorTranslation[ T ] ) : TapirValidator[ T ] = {
    tr.translate( validator )
  }

  def translateValidators[ T ](
    validators: Set[ Validator[ T ] ],
  )(
    using
    tr : TapirValidatorTranslation[ T ],
  ) : TapirValidator[ T ] = {
    validators.foldLeft( TapirValidator.pass[ T ] ){ ( lastTv, nextv ) =>
      lastTv.and( tr.translate( nextv ) )
    }
  }
}
