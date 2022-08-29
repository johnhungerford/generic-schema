package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.types.Nat
import org.hungerford.generic.schema.{Component, utilities}

type Append[ R <: Tuple, T ] = Tuple.Concat[ R, T *: EmptyTuple ]

sealed trait Dir
sealed trait Forward extends Dir
sealed trait Backward extends Dir

sealed trait Pred
sealed trait CanMigrate extends Pred
sealed trait TypeEquiv extends Pred
sealed trait NameEquivCanMigrate extends Pred
sealed trait NameTypeEquiv extends Pred

trait AlignFilter[D <: Dir, RV1, R1, Outer1, RV2, R2, Outer2] {
	type ROut
	type RVOut

	def alignComponents(r1: R1): ROut
	def alignValues(rv1: RV1): RVOut
}

object AlignFilter {
	type Aux[D <: Dir, RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO] = AlignFilter[D, RV1, R1, Outer1, RV2, R2, Outer2] { type ROut = RO; type RVOut = RVO }
	given nameTypeAlign[D <: Dir, RV1 <: Tuple, R1 <: Tuple, ZI1 <: Tuple, Outer1, RV2 <: Tuple, R2 <: Tuple, Z2 <: Tuple, Outer2, Mone <: Tuple, ZI1one <: Tuple, Mtwo <: Tuple, ZI1two <: Tuple, Mthree <: Tuple, ZI1three <: Tuple, Mfour <: Tuple, ZI1four <: Tuple, RVO <: Tuple, RO <: Tuple](
		using
		zi1: ZipRsWithIndex.Aux[Nat._0, RV1, R1, ZI1],
		z2: ZipRs.Aux[RV2, R2, Z2],
		one: MapAlign.Aux[D, NameTypeEquiv, ZI1, Outer1, Z2, Outer2, Nothing, Mone, ZI1one],
		two: MapAlign.Aux[D, NameEquivCanMigrate, ZI1one, Outer1, Z2, Outer2, Mone, Mtwo, ZI1two],
		three: MapAlign.Aux[D, TypeEquiv, ZI1two, Outer1, Z2, Outer2, Mtwo, Mthree, ZI1three],
		four: MapAlign.Aux[D, CanMigrate, ZI1three, Outer1, Z2, Outer2, Mthree, Mfour, ZI1four],
		afm: AlignFromMap.Aux[Mfour, RV1, R1, RVO, RO]
	): AlignFilter[D, RV1, R1, Outer1, RV2, R2, Outer2] with {
		type RVOut = RVO
		type ROut = RO
		def alignComponents(r1: R1): ROut =
			afm.alignComponents(r1)

		def alignValues(rv1: RV1): RVOut =
			afm.alignValues(rv1)
	}
}

trait ZipRs[RV <: Tuple, R <: Tuple] {
	type Zipped <: Tuple
}

object ZipRs {
	type Aux[RV <: Tuple, R <: Tuple, Z <: Tuple] = ZipRs[RV, R] { type Zipped = Z }

	given empty: ZipRs[EmptyTuple, EmptyTuple] with {
		type Zipped = EmptyTuple
	}

	given next[HRV, TRV <: Tuple, HR, TR <: Tuple, NextZ <: Tuple](
		using
		nextZ: ZipRs.Aux[TRV, TR, NextZ],
	): ZipRs[HRV *: TRV, HR *: TR] with {
		type Zipped = (HRV, HR) *: NextZ
	}
}

trait ZipRsWithIndex[I0 <: Nat, RV <: Tuple, R <: Tuple] {
	type Zipped <: Tuple
}

object ZipRsWithIndex {
	type Aux[I0 <: Nat, RV <: Tuple, R <: Tuple, Z <: Tuple] = ZipRsWithIndex[I0, RV, R] { type Zipped = Z }

	given empty[I <: Nat]: ZipRsWithIndex[I, EmptyTuple, EmptyTuple] with {
		type Zipped = EmptyTuple
	}

	given next[I0 <: Nat, HRV, TRV <: Tuple, HR, TR <: Tuple, NextZ <: Tuple](
		using
		nextZ: ZipRsWithIndex.Aux[Nat.Succ[I0], TRV, TR, NextZ],
	): ZipRsWithIndex[I0, HRV *: TRV, HR *: TR] with {
		type Zipped = (HRV, HR, I0) *: NextZ
	}
}

trait MapAlign[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, Z2 <: Tuple, O2, MapIn] {
	type MapOut <: Tuple
	type NewZI1 <: Tuple
}

trait MapAligns1 {
	given skip[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, HZ2, TZ2 <: Tuple, O2, HMI <: Nat, TMI <: Tuple, FoundI <: Nat | Nothing, NextMO <: Tuple, NextZI1 <: Tuple] (
		using
		nextMa: MapAlign.Aux[D, P, ZI1, O1, TZ2, O2, TMI, NextMO, NextZI1]
	): MapAlign[D, P, ZI1, O1, HZ2 *: TZ2, O2, HMI *: TMI] with {
		type MapOut = HMI *: NextMO
		type NewZI1 = NextZI1
	}

	given noZI1MITup[D <: Dir, P <: Pred, O1, Z2 <: Tuple, O2, MI <: Tuple]: MapAlign[D, P, EmptyTuple, O1, Z2, O2, MI] with {
		type MapOut = MI
		type NewZI1 = EmptyTuple
	}

	given noZI1MINothing[D <: Dir, P <: Pred, O1, HZ2, TZ2 <: Tuple, O2, NextMO <: Tuple](
		using
		next: MapAlign.Aux[D, P, EmptyTuple, O1, TZ2, O2, Nothing, NextMO, EmptyTuple]
	): MapAlign[D, P, EmptyTuple, O1, HZ2 *: TZ2, O2, Nothing] with {
		type MapOut = Nothing *: NextMO
		type NewZI1 = EmptyTuple
	}
}

object MapAlign extends MapAligns1 {
	type Aux[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, Z2 <: Tuple, O2, MI, MO <: Tuple, NZI1 <: Tuple] =
		MapAlign[D, P, ZI1, O1, Z2, O2, MI] { type MapOut = MO; type NewZI1 = NZI1 }

	given emptyZ2[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, O2, MI]: MapAlign[D, P, ZI1, O1, EmptyTuple, O2, MI] with {
		type MapOut = EmptyTuple
		type NewZI1 = ZI1
	}

	given nextMI[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, HZ2V, HZ2S, TZ2 <: Tuple, O2, HMI <: Nat | Nothing, TMI <: Tuple, FoundI <: Nat | Nothing, UpdatedZI1 <: Tuple, NextMO <: Tuple, NextZI1 <: Tuple](
		using
		find: FindAndRemove.Aux[D, P, HZ2V, HZ2S, O1, ZI1, O2, FoundI, UpdatedZI1],
		nextMa: MapAlign.Aux[D, P, UpdatedZI1, O1, TZ2, O2, TMI, NextMO, NextZI1]
	): MapAlign[D, P, ZI1, O1, (HZ2V, HZ2S) *: TZ2, O2, Nothing *: TMI] with {
		type MapOut = FoundI *: NextMO
		type NewZI1 = NextZI1
	}

	given next[D <: Dir, P <: Pred, ZI1 <: Tuple, O1, HZ2V, HZ2S, TZ2 <: Tuple, O2, FoundI <: Nat | Nothing, UpdatedZI1 <: Tuple, NextMO <: Tuple, NextZI1 <: Tuple] (
		using
		find: FindAndRemove.Aux[D, P, HZ2V, HZ2S, O1, ZI1, O2, FoundI, UpdatedZI1],
		nextMa: MapAlign.Aux[D, P, UpdatedZI1, O1, TZ2, O2, Nothing, NextMO, NextZI1]
	): MapAlign[D, P, ZI1, O1, (HZ2V, HZ2S) *: TZ2, O2, Nothing] with {
		type MapOut = FoundI *: NextMO
		type NewZI1 = NextZI1
	}
}

trait FindAndRemove[D <: Dir, P <: Pred, V, S, O1, ZI <: Tuple, O2] {
	type Index <: Nat | Nothing
	type NewZI <: Tuple
}

trait FindAndRemoves1 {
	given tryAgain[D <: Dir, P <: Pred, V, S, O1, HZI, TZI <: Tuple, O2, I <: Nat | Nothing, NextZI <: Tuple](
		using
		next: FindAndRemove.Aux[D, P, V, S, O1, TZI, O2, I, NextZI],
	): FindAndRemove[D, P, V, S, O1, HZI *: TZI, O2] with {
		type Index = I
		type NewZI = HZI *: NextZI
	}
}

object FindAndRemove extends FindAndRemoves1 {
	type Aux[D <: Dir, P <: Pred, V, S, O1, ZI <: Tuple, O2, I <: Nat | Nothing, NZI <: Tuple] =
		FindAndRemove[D, P, V, S, O1, ZI, O2] { type Index = I; type NewZI = NZI }

	given notFound[D <: Dir, P <: Pred, V, S, O1, O2]: FindAndRemove[D, P, V, S, O1, EmptyTuple, O2] with {
		type Index = Nothing
		type NewZI = EmptyTuple
	}

	given foundForward[P <: Pred, V, S, O1, V2, S2, I2 <: Nat, TZI <: Tuple, O2](
		using
		ap: AlignPredicate[P, V2, S2, O2, V, S, O1],
	): FindAndRemove[Forward, P, V, S, O1, (V2, S2, I2) *: TZI, O2] with {
		type Index = I2
		type NewZI = TZI
	}

	given foundBackward[ P <: Pred, V, S, O1, V2, S2, I2 <: Nat, TZI <: Tuple, O2] (
		using
		sm: AlignPredicate[P, V, S, O1, V2, S2, O2],
	): FindAndRemove[Backward, P, V, S, O1, (V2, S2, I2) *: TZI, O2] with {
		type Index = I2
		type NewZI = TZI
	}
}

trait AlignFromMap[Map <: Tuple, RV <: Tuple, R <: Tuple] {
	type RVOut <: Tuple
	type ROut <: Tuple
	def alignComponents(r: R): ROut
	def alignValues(rv: RV): RVOut
}

object AlignFromMap {
	type Aux[Map <: Tuple, RV <: Tuple, R <: Tuple, RVO <: Tuple, RO <: Tuple] =
		AlignFromMap[Map, RV, R] { type RVOut = RVO; type ROut = RO }

	given finished[CI <: Nat, RV <: Tuple, R <: Tuple]: AlignFromMap[EmptyTuple, RV, R] with {
		type RVOut = EmptyTuple
		type ROut = EmptyTuple
		def alignComponents(r: R): EmptyTuple = EmptyTuple
		def alignValues(rv: RV): EmptyTuple = EmptyTuple
	}

	given getNext[HMap <: Nat, TMap <: Tuple, RV <: Tuple, R <: Tuple, V, S, RVO <: Tuple, RO <: Tuple](
		using
		getIndexRV: GetIndex.Aux[HMap, RV, Nat._0, V],
		getIndexR: GetIndex.Aux[HMap, R, Nat._0, S],
		next: AlignFromMap.Aux[TMap, RV, R, RVO, RO],
	): AlignFromMap[HMap *: TMap, RV, R] with {
		type RVOut = V *: RVO
		type ROut = S *: RO
		def alignComponents(r: R): S *: RO = getIndexR.get(r) *: next.alignComponents(r)
		def alignValues(rv: RV): V *: RVO = getIndexRV.get(rv) *: next.alignValues(rv)
	}
}

trait GetIndex[I <: Nat, R <: Tuple, Start <: Nat] {
	type T

	def get(r: R): T
}

object GetIndex {
	type Aux[I <: Nat, R <: Tuple, Start <: Nat, TO] = GetIndex[I, R, Start] { type T = TO }

	given atIndex[I <: Nat, HR, TR <: Tuple]: GetIndex[I, HR *: TR, I] with {
		type T = HR
		def get(r: HR *: TR): HR = r.head
	}

	given next[I <: Nat, HR, TR <: Tuple, Current <: Nat, TO](
		using
		next: GetIndex.Aux[I, TR, Nat.Succ[Current], TO],
	): GetIndex[I, HR *: TR, Current] with {
		type T = TO
		def get(r: HR *: TR): TO = next.get(r.tail)
	}
}

//trait AlignAll[D <: Dir, P <: Pred, RV1 <: Tuple, R1 <: Tuple, O1, RV2 <: Tuple, R2 <: Tuple, O2] {
//	type ROut
//	type RVOut
//
//	def alignComponents(r1: R1): ROut
//	def alignValues(rv1: RV1): RVOut
//}
//
//trait AlignAlls1 {
//	given filter[D <: Dir, P <: Pred, RV1 <: Tuple, R1 <: Tuple, O1, O2]: AlignAll[D, P, RV1, R1, O1, EmptyTuple, EmptyTuple, O2] with {
//		type ROut = EmptyTuple
//		type RVOut = EmptyTuple
//
//		def alignComponents(r1: R1): EmptyTuple = EmptyTuple
//
//		def alignValues(rv1: RV1): EmptyTuple = EmptyTuple
//	}
//}
//
//object AlignAll extends AlignAlls1 {
//	type Aux[D <: Dir, P <: Pred, RV1 <: Tuple, R1 <: Tuple, O1, RV2 <: Tuple, R2 <: Tuple, O2, RVO, RO] = AlignAll[D, P, RV1, R1, O1, RV2, R2, O2] { type RVOut = RVO; type ROut = RO }
//
//	given alignNextSuccess[D <: Dir, P <: Pred, RV1 <: NonEmptyTuple, R1 <: NonEmptyTuple, O1, HRV2, TRV2 <: Tuple, HR2, TR2 <: Tuple, O2, HNRV1, TNRV1 <: Tuple, HNR1, TNR1 <: Tuple, RVO <: Tuple, RO <: Tuple](
//		using
//		once: AlignOnce.AuxS[D, P, EmptyTuple, RV1, EmptyTuple, R1, O1, HRV2, HR2, O2, HNRV1, TNRV1, HNR1, TNR1, Success],
//		next: AlignAll.Aux[D, P, TNRV1, TNR1, O1, TRV2, TR2, O2, RVO, RO],
//	): AlignAll[D, P, RV1, R1, O1, HRV2 *: TRV2, HR2 *: TR2, O2] with {
//		type RVOut = HNRV1 *: RVO
//		type ROut = HNR1 *: RO
//
//		def alignComponents(r1: R1): HNR1 *: RO =
//			val onceAligned = once.alignComponents(r1, EmptyTuple)
//			val hnr1: HNR1 = onceAligned.head
//			val tnr1: TNR1 = onceAligned.tail
//			hnr1 *: next.alignComponents(tnr1)
//
//		def alignValues(rv1: RV1): HNRV1 *: RVO =
//			val onceAligned: HNRV1 *: TNRV1 = once.alignValues(rv1, EmptyTuple)
//			val hnrv1: HNRV1 = onceAligned.head
//			val tnrv1: TNRV1 = onceAligned.tail
//			hnrv1 *: next.alignValues(tnrv1)
//	}
//
//	given alignNextFailure[D <: Dir, P <: Pred, RV1 <: NonEmptyTuple, R1 <: NonEmptyTuple, O1, HRV2, TRV2 <: Tuple, HR2, TR2 <: Tuple, O2, HNRV1, TNRV1 <: Tuple, HNR1, TNR1 <: Tuple, RVO <: Tuple, RO <: Tuple] (
//		using
//		once: AlignOnce.AuxS[D, P, EmptyTuple, RV1, EmptyTuple, R1, O1, HRV2, HR2, O2, HNRV1, TNRV1, HNR1, TNR1, Failure],
//		next: AlignAll.Aux[D, P, HNRV1 *: TNRV1, HNR1 *: TNR1, O1, TRV2, TR2, O2, RVO, RO],
//	): AlignAll[D, P, RV1, R1, O1, HRV2 *: TRV2, HR2 *: TR2, O2] with {
//		type RVOut = RVO
//		type ROut = RO
//
//		def alignComponents(r1: R1): RO =
//			val onceAligned = once.alignComponents(r1, EmptyTuple)
//			next.alignComponents(onceAligned)
//
//		def alignValues(rv1: RV1): RVO =
//			val onceAligned: HNRV1 *: TNRV1 = once.alignValues(rv1, EmptyTuple)
//			next.alignValues(onceAligned)
//	}
//
//	given done[D <: Dir, P <: Pred, RV2 <: Tuple, R2 <: Tuple, O1, O2]: AlignAll[D, P, EmptyTuple, EmptyTuple, O1, RV2, R2, O2] with {
//		type ROut = EmptyTuple
//		type RVOut = EmptyTuple
//
//		def alignComponents(r1: EmptyTuple): EmptyTuple = EmptyTuple
//		def alignValues(rv1: EmptyTuple): EmptyTuple = EmptyTuple
//	}
//}
//
//sealed trait Status
//sealed trait Success extends Status
//sealed trait Failure extends Status
//
//trait AlignOnce[D <: Dir, P <: Pred, UsedRV1 <: Tuple, RV1 <: Tuple, UsedR1 <: Tuple, R1 <: Tuple, Out1, V2, S2, Out2] {
//	type ROutH
//	type ROutT <: Tuple
//	type ROut = ROutH *: ROutT
//	type RVOutH
//	type RVOutT <: Tuple
//	type RVOut = RVOutH *: RVOutT
//	type Stat <: Status
//
//	def alignComponents(r1: R1, usedR1: UsedR1): ROut
//
//	def alignValues(rv1: RV1, usedRv1: UsedRV1): RVOut
//}
//
//trait AlignOnces3 {
//
////	given backup[D <: Dir, P <: Pred, HURV, TURV <: Tuple, HUR, TUR <: Tuple, O1, RV2 <: Tuple, R2 <: Tuple, O2, RVO <: Tuple, RO <: Tuple](
////		using
////		next: MigrationAlignFilter.Aux[D, P, EmptyTuple, TURV, EmptyTuple, TUR, O1, RV2, R2, O2, RVO, RO],
////	): MigrationAlignFilter[D, P, HURV *: TURV, EmptyTuple, HUR *: TUR, EmptyTuple, O1, RV2, R2, O2] with {
////		type RVOut = HURV *: RVO
////		type ROut = HUR *: RO
////
////		override def alignComponents(r1: EmptyTuple, usedR1: HUR *: TUR): HUR *: RO =
////			val tur: TUR = usedR1.tail
////			usedR1.head *: next.alignComponents(tur, EmptyTuple)
////		override def alignValues(rv1: EmptyTuple, usedRv1: HURV *: TURV): HURV *: RVO =
////			val turv: TURV = usedRv1.tail
////			usedRv1.head *: next.alignValues(turv, EmptyTuple)
////	}
//}
//
//trait AlignOnces2 extends AlignOnces3 {
//	given unaligned[D <: Dir, P <: Pred, URV1 <: Tuple, HRV1, TRV1 <: Tuple, UR1 <: Tuple, HR1, TR1 <: Tuple, Outer1, V2, S2, Outer2, HNewRV, TNewRV <: Tuple, HNewR, TNewR <: Tuple, S <: Status](
//		using
//		next: AlignOnce.AuxS[D, P, Append[URV1, HRV1], TRV1, Append[UR1, HR1], TR1, Outer1, V2, S2, Outer2, HNewRV, TNewRV, HNewR, TNewR, S],
//	): AlignOnce[D, P, URV1, HRV1 *: TRV1, UR1, HR1 *: TR1, Outer1, V2, S2, Outer2] with {
//		type RVOutH = HNewRV
//		type RVOutT = TNewRV
//		type ROutH = HNewR
//		type ROutT = TNewR
//		type Stat = S
//
//		def alignComponents(r1: HR1 *: TR1, usedR1: UR1): ROut =
//			val newUR1: Append[UR1, HR1] = usedR1 ++ (r1.head *: EmptyTuple)
//			val newR1 : TR1 = r1.tail
//			next.alignComponents(newR1, newUR1)
//		def alignValues(r1: HRV1 *: TRV1, usedR1: URV1): RVOut =
//			val newURV1: Append[URV1, HRV1] = usedR1 ++ (r1.head *: EmptyTuple)
//			val newRV1: TRV1 = r1.tail
//			next.alignValues(newRV1, newURV1)
//	}
//}
//
//trait AlignOnces1 extends AlignOnces2 {
//	given alignedForward[P <: Pred, URV1 <: Tuple, HRV1, TRV1 <: Tuple, UR1 <: Tuple, HR1 <: Component.Tpe[HRV1], TR1 <: Tuple, Outer1, V2, S2 <: Component.Tpe[V2], Outer2] (
//		using
//		pred: AlignPredicate[P, HRV1, HR1, Outer1, V2, S2, Outer2],
//	): AlignOnce[Forward, P, URV1, HRV1 *: TRV1, UR1, HR1 *: TR1, Outer1, V2, S2, Outer2] with {
//		type RVOutH = HRV1
//		type RVOutT = Tuple.Concat[URV1, TRV1]
//		type ROutH = HR1
//		type ROutT = Tuple.Concat[UR1, TR1]
//		type Stat = Success
//
//		def alignComponents(r1: HR1 *: TR1, ur1: UR1): ROut =
//			val hr1: HR1 = r1.head
//			val tr1: TR1 = r1.tail
//			hr1 *: (ur1 ++ tr1)
//
//		def alignValues(rv1: HRV1 *: TRV1, urv1: URV1): RVOut =
//			val hrv1: HRV1 = rv1.head
//			val trv1: TRV1 = rv1.tail
//			hrv1 *: (urv1 ++ trv1)
//	}
//
//	given alignedBackward[P <: Pred, URV1 <: Tuple, HRV1, TRV1 <: Tuple, UR1 <: Tuple, HR1 <: Component.Tpe[HRV1], TR1 <: Tuple, Outer1, V2, S2 <: Component.Tpe[V2], Outer2] (
//		using
//		pred: AlignPredicate[P, V2, S2, Outer2, HRV1, HR1, Outer1],
//	): AlignOnce[Backward, P, URV1, HRV1 *: TRV1, UR1, HR1 *: TR1, Outer1, V2, S2, Outer2] with {
//		type RVOutH = HRV1
//		type RVOutT = Tuple.Concat[URV1, TRV1]
//		type ROutH = HR1
//		type ROutT = Tuple.Concat[UR1, TR1]
//		type Stat = Success
//
//		def alignComponents(r1: HR1 *: TR1, ur1: UR1): ROut =
//			val hr1: HR1 = r1.head
//			val tr1: TR1 = r1.tail
//			hr1 *: (ur1 ++ tr1)
//
//		def alignValues(rv1: HRV1 *: TRV1, urv1: URV1): RVOut =
//			val hrv1: HRV1 = rv1.head
//			val trv1: TRV1 = rv1.tail
//			hrv1 *: (urv1 ++ trv1)
//	}
//
//}
//
//object AlignOnce extends AlignOnces1 {
//	type Aux[D <: Dir, P <: Pred, UsedRV1 <: Tuple, RV1 <: Tuple, UsedR1 <: Tuple, R1 <: Tuple, Outer1, V2, S2, Outer2, RVOH, RVOT <: Tuple, ROH, ROT <: Tuple] = AlignOnce[D, P, UsedRV1, RV1, UsedR1, R1, Outer1, V2, S2, Outer2] {
//		type ROutH = ROH
//		type ROutT = ROT
//		type RVOutH = RVOH
//		type RVOutT = RVOT
//	}
//	type AuxS[D <: Dir, P <: Pred, UsedRV1 <: Tuple, RV1 <: Tuple, UsedR1 <: Tuple, R1 <: Tuple, Outer1, V2, S2, Outer2, RVOH, RVOT <: Tuple, ROH, ROT <: Tuple, S <: Status] = AlignOnce[D, P, UsedRV1, RV1, UsedR1, R1, Outer1, V2, S2, Outer2] {
//		type ROutH = ROH
//		type ROutT = ROT
//		type RVOutH = RVOH
//		type RVOutT = RVOT
//		type Stat = S
//	}
//
//	given emptyAligned[D <: Dir, P <: Pred, HURV1, TURV1 <: Tuple, HUR1, TUR1 <: Tuple, V2, S2, Outer1, Outer2]: AlignOnce[D, P, HURV1 *: TURV1, EmptyTuple, HUR1 *: TUR1, EmptyTuple, Outer1, V2, S2, Outer2] with {
//		type RVOutH = HURV1
//		type RVOutT = TURV1
//		type ROutH = HUR1
//		type ROutT = TUR1
//		type Stat = Failure
//
//		def alignComponents(r1: EmptyTuple, usedR1: HUR1 *: TUR1): ROut = usedR1
//		def alignValues(rv1: EmptyTuple, usedRV1: HURV1 *: TURV1): RVOut = usedRV1
//	}
//}

trait AlignPredicate[P <: Pred, V1, S1, O1, V2, S2, O2]

object AlignPredicate {
	given canMigratePredicate[V1, S1, O1, V2, S2, O2](
		using
		cm: ShapeMigration.Aux[V1, S1, O1, V2, S2, O2],
	): AlignPredicate[CanMigrate, V1, S1, O1, V2, S2, O2] with {}

	given typeEquiv[V, S1 <: Component.Tpe[V], S2 <: Component.Tpe[V], O1, O2](
		using
		cm: ShapeMigration.Aux[V, S1, O1, V, S2, O2],
	): AlignPredicate[TypeEquiv, V, S1, O1, V, S2, O2] with {}

	given nameEquivCanMigrate[V1, N <: String & Singleton, S1 <: Component.Named[N], O1, V2, S2 <: Component.Named[N], O2](
		using
		cm: ShapeMigration.Aux[V1, S1, O1, V2, S2, O2],
	): AlignPredicate[NameEquivCanMigrate, V1, S1, O2, V2, S2, O2] with {}

	given nameTypeEquiv[V, N <: String & Singleton, S1 <: Component.Tpe[V] & Component.Named[N], O1, S2 <: Component.Tpe[V] & Component.Named[N], O2](
		using
		cm: ShapeMigration.Aux[V, S1, O1, V, S2, O2],
	):
	  AlignPredicate[NameTypeEquiv, V, S1, O1, V, S2, O2] with {}
}
