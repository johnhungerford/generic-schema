package org.hungerford.generic.schema.types

import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.coproduct.subtype.Subtype
import org.hungerford.generic.schema.product.field.Field

// X is some element (field, subtype) of type T
trait Of[X, T]

object Of {
	given subtype[T, ST <: Subtype.Tpe[T]]: Of[ST, T] with {}
	given field[T, F <: Field.Tpe[T]]: Of[F, T] with {}
}

trait OfName[X, N <: String & Singleton] {
	def nameOf(named: X): N
}

object OfName {
	given subtype[N <: String & Singleton, ST <: Subtype.Named[N]]: OfName[ST, N] with {
		def nameOf(named: ST): N = named.typeName
	}
	given field[N <: String & Singleton, F <: Field.Named[N]]: OfName[F, N] with {
		def nameOf(named: F): N = named.fieldName
	}
}

trait OfShape[X, T, S, OuterT] {
	def shapeOf(shaped: X): S
}

object OfShape {
	given subtype[T, S, OuterT, ST <: Subtype.Shaped[T, S]]: OfShape[ST, T, S, OuterT] with {
		def shapeOf(shaped: ST): S = shaped.schema.shape
	}
	given lazySubtype[OuterT, OuterS, T, ST <: Subtype.Tpe[T] & Subtype.IsLazy, S](
		using
		outerSch: Schema.Aux[OuterT, OuterS],
		schExt: SchemaExtractor.Aux[T, Schema.Aux[OuterT, OuterS], S],
	): OfShape[ST, T, S, OuterT] with {
		def shapeOf(shaped: ST): S = schExt.extract(outerSch).shape
	}
	given field[T, S, OuterT, F <: Field.Shaped[T, S]]: OfShape[F, T, S, OuterT] with {
		def shapeOf(shaped: F): S = shaped.schema.shape
	}
	given lazyField[OuterT, OuterS, T, F <: Field.Tpe[T] & Field.IsLazy, S](
		using
		outerSch: Schema.Aux[OuterT, OuterS],
		schExt: SchemaExtractor.Aux[T, Schema.Aux[OuterT, OuterS], S]
	): OfShape[F, T, S, OuterT] with {
		def shapeOf(shaped: F): S = schExt.extract(outerSch).shape
	}
}
