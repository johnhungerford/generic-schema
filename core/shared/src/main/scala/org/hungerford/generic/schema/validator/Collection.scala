package org.hungerford.generic.schema.validator

trait Collection[Col] {
	type OfType
	def iterable(collection: Col): Iterable[OfType]
}

object Collection {
	type Aux[Col, T] = Collection[Col] { type OfType = T }

	transparent inline def apply[Col : Collection] = summon[Collection[Col]]

	given iterable[T, Col <: Iterable[T]]: Collection[Col] with {
		type OfType = T
		def iterable(collection: Col): Iterable[T] = collection
	}

	given string: Collection[String] with {
		type OfType = Char
		def iterable(collection: String): Iterable[Char] = collection.toIterable
	}

	given array[T]: Collection[Array[T]] with {
		override type OfType = T
		def iterable(collection: Array[T]): Iterable[T] = collection.toIterable
	}
}
