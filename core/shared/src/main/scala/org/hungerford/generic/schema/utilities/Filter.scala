//package org.hungerford.generic.schema.utilities
//
//import org.hungerford.generic.schema.coproduct.subtype.Subtype
//import org.hungerford.generic.schema.product.field.Field
//
///**
// * Keeps elements of RV/R tuples when there exists a migration between them
// */
//trait Filter[RV1, R1, O1, RV2, R2, O2] {
//	type RVOut
//	type ROut
//	def filterComponents(r1: R1): ROut
//	def filterValues(rv1: RV1): RVOut
//}
//
//trait Filters1 {
//	given filteredFilter[HRV1, TRV1 <: Tuple, HR1, TR1 <: Tuple, O1, RV2 <: Tuple, R2 <: Tuple, O2, RVO <: Tuple, RO <: Tuple](
//		using
//		nextFilter: Filter.Aux[TRV1, TR1, O1, RV2, R2, O2, RVO, RO],
//	): Filter[HRV1 *: TRV1, HR1 *: TR1, O1, RV2, R2, O2] with {
//		type RVOut = RVO
//		type ROut = RO
//
//		def filterComponents(r1: HR1 *: TR1): RO = nextFilter.filterValues(r1.tail)
//		def filterValues(rv1: HRV1 *: TRV1): RVO = nextFilter.filterComponents(rv1.tail)
//	}
//}
//
//object Filter extends Filters1 {
//	type Aux[RV1, R1, O1, RV2, R2, O2, RVO, RO] = Filter[RV1, R1, O1, RV2, R2, O2] { type RVOut = RVO; type ROut = RO }
//
//	given emptyFilter[RV2 <: Tuple, R2 <: Tuple, O1, O2]: Filter[EmptyTuple, EmptyTuple, O1, RV2, R2, O2] with {
//		type RVOut = EmptyTuple
//		type ROut = EmptyTuple
//
//		override def filterComponents(r1: EmptyTuple): EmptyTuple = EmptyTuple
//		override def filterValues(rv1: EmptyTuple): EmptyTuple = EmptyTuple
//	}
//
//	given nonEmptyFilter[HRV1, TRV1 <: Tuple, HR1, TR1 <: Tuple, O1, RV2 <: Tuple, R2 <: Tuple, O2, NextRV2 <: Tuple, NextR2 <: Tuple, RVO <: Tuple, RO <: Tuple](
//		using
//		wo: WithoutOnce.Aux[RV2, R2, O2, HRV1, HR1, O1, NextRV2, NextR2],
//		next: Filter.Aux[TRV1, TR1, O1, RV2, R2, O2, RVO, RO],
//	): Filter[HRV1 8] with {
//		type ROut = H *: Res
//		def filter(r1: H *: Tail, r2: R2): H *: Res =
//			val filteredR2 = wo.remove(r2)
//			r1.head *: next.filter(r1.tail, filteredR2)
//	}
//}
//
//trait WithoutOnce[RV <: Tuple, R <: Tuple, OuterR, T, C, OuterC] {
//	type RVOut
//	type ROut
//	def removeComponents(from: R): ROut
//	def removeValues(from: RV): RVOut
//}
//
//trait WithoutOnces1 {
//	given next[HRV, TRV <: Tuple, HR, TR <: Tuple, OR, T, C, OC, NextRV <: Tuple, NextR <: Tuple](
//		using
//		next: WithoutOnce.Aux[TRV, TR, OR, T, C, OC, NextRV, NextR],
//	): WithoutOnce[HRV *: TRV, HR *: TR, OR, T, C, OC] with {
//		type RVOut = HRV *: NextRV
//		type ROut = HR *: NextR
//		def removeComponents(from: HR *: TR): ROut =
//			from.head *: next.removeComponents(from.tail)
//		def removeValues(from: HRV *: TRV): RVOut =
//			from.head *: next.removeValues(from.tail)
//	}
//}
//
//object WithoutOnce extends WithoutOnces1 {
//	type Aux[RV <: Tuple, R <: Tuple, OR, T, C, OC, RVO, RO] = WithoutOnce[RV, R, OR, T, C, OC] { type RVOut = RVO; type ROut = RO }
//
//	given removeIt[HRV, TRV <: Tuple, HR, TR <: Tuple, OR, T, C, OC, NextRV <: Tuple, NextR <: Tuple](
//		using
//		sm: ShapeMigration.Aux[T, C, OC, HRV, HR, OR],
//	): WithoutOnce[HRV *: TRV, HR *: TR, OR, T, C, OC] with {
//		type RVOut = NextRV
//		type ROut = NextR
//
//		def removeComponents(from: HR *: TR): ROut =
//			next.removeComponents(from.tail)
//
//		def removeValues(from: HRV *: TRV): RVOut =
//			next.removeValues(from.tail)
//	}
//}
//
