package org.hungerford.generic.schema.types

import org.hungerford.generic.schema.coproduct.subtype.Subtype
import org.hungerford.generic.schema.product.field.Field

trait Filter[R1, R2, Predicate[_, _]] {
	type ROut
	def filter(r1: R1, r2: R2): ROut
}

trait Filters1 {
	given filteredFilter[H1, T1 <: Tuple, R2 <: Tuple, Predicate[_, _], Res <: Tuple](
		using
		nextFilter: Filter.Aux[T1, R2, Predicate, Res],
	): Filter[H1 *: T1, R2, Predicate] with {
		type ROut = Res
		def filter(r1: H1 *: T1, r2: R2): Res = nextFilter.filter(r1.tail, r2)
	}
}

object Filter extends Filters1 {
	type Aux[R1, R2, Predicate[_, _], RO] = Filter[R1, R2, Predicate] { type ROut = RO }

	given emptyFilter[R2, Predicate[_, _]]: Filter[EmptyTuple, R2, Predicate] with {
		type ROut = EmptyTuple
		def filter(r1: EmptyTuple, r2: R2): EmptyTuple = EmptyTuple
	}

	given nonEmptyFilter[HT, H, Tail <: Tuple, R2 <: Tuple, Predicate[_, _], NextR2 <: Tuple, Res <: Tuple](
		using
		wo: Without.Aux[R2, HT, Predicate, NextR2],
		next: Filter.Aux[Tail, NextR2, Predicate, Res],
	): Filter[H *: Tail, R2, Predicate] with {
		type ROut = H *: Res
		def filter(r1: H *: Tail, r2: R2): H *: Res =
			val filteredR2 = wo.remove(r2)
			r1.head *: next.filter(r1.tail, filteredR2)
	}
}

trait Without[R <: Tuple, T, Predicate[_, _]] {
	type Out
	def remove(from: R): Out
}

trait Withouts1 {
	given next[T, H, Tail <: Tuple, Predicate[_, _], Res <: Tuple](
		using
		next: Without.Aux[Tail, T, Predicate, Res],
	): Without[H *: Tail, T, Predicate] with {
		type Out = H *: Res
		def remove(from: H *: Tail): H *: Res = from.head *: next.remove(from.tail)
	}
}

object Without {
	type Aux[R <: Tuple, T, Predicate[_, _], O] = Without[R, T, Predicate] { type Out = O }

	given removeIt[T, H, Tail <: Tuple, Predicate[_, _]](
		using
		satisfiesPredicate: Predicate[T, H],
	): Without[H *: Tail, T, Predicate] with {
		type Out = Tail
		def remove(from: H *: Tail): Tail = from.tail
	}
}
