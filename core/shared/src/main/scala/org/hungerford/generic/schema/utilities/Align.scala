package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.{Component, utilities}

type Append[ R <: Tuple, T ] = Tuple.Concat[ R, T *: EmptyTuple ]

trait Align[RV1, R1, Outer1, RV2, R2, Outer2] {
	type ROut
	type RVOut

	def alignComponents(r1: R1): ROut
	def alignValues(rv1: RV1): RVOut
}

object Align {
	type Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO] = Align[RV1, R1, Outer1, RV2, R2, Outer2] { type ROut = RO; type RVOut = RVO }
	given nameTypeAlign[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO](
		using nta: NameMigrationAlign.Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO],
	): Align.Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO] = nta
}

trait MigrationAlign[RV1, R1, Out1, RV2, R2, Out2] extends Align[RV1, R1, Out1, RV2, R2, Out2]

trait MigrationAligns1 {
	given unaligned[HRV1, TRV1 <: Tuple, HR1, TR1 <: Tuple, Outer1, RV2 <: Tuple, R2 <: Tuple, Outer2, NewRV, NewR](
		using
		next: NameMigrationAlign.Aux[Append[TRV1, HRV1], Append[TR1, HR1], Outer1, RV2, R2, Outer2, NewRV, NewR],
	): MigrationAlign[HRV1 *: TRV1, HR1 *: TR1, Outer1, RV2, R2, Outer2] with {
		type RVOut = NewRV
		type ROut = NewR


		def alignComponents(r1: HR1 *: TR1): NewR =
			val t : TR1 = r1.tail
			val hTup : HR1 *: EmptyTuple = r1.head *: EmptyTuple
			next.alignComponents((t ++ hTup).asInstanceOf[Append[TR1, HR1]])
		def alignValues(rv1: HRV1 *: TRV1): NewRV =
			val t: TRV1 = rv1.tail
			val hTup: HRV1 *: EmptyTuple = rv1.head *: EmptyTuple
			next.alignValues((t ++ hTup).asInstanceOf[Append[TRV1, HRV1]])
	}
}

object MigrationAlign extends MigrationAligns1 {
	type Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO] = MigrationAlign[RV1, R1, Outer1, RV2, R2, Outer2] { type RVOut = RVO; type ROut = RO }

	given aligned[HRV1, TRV1 <: Tuple, HR1 <: Component.Tpe[HRV1], TR1 <: Tuple, Outer1, HRV2, TRV2 <: Tuple, HR2 <: Component.Tpe[HRV2], TR2 <: Tuple, Outer2, AlignedTRV <: Tuple, AlignedTR <: Tuple](
		using
		sm: ShapeMigration.Aux[HRV1, HR1, Outer1, HRV2, HR2, Outer2],
		nextAlign: NameMigrationAlign.Aux[TRV1, TR1, Outer1, TRV2, TR2, Outer2, AlignedTRV, AlignedTR],
	): MigrationAlign[HRV1 *: TRV1, HR1 *: TR1, Outer1, HRV2 *: TRV2, HR2 *: TR2, Outer2] with {
		type RVOut = HRV1 *: AlignedTRV
		type ROut = HR1 *: AlignedTR

		def alignComponents(r1: HR1 *: TR1): ROut =
			r1.head *: nextAlign.alignComponents(r1.tail)
		def alignValues(rv1: HRV1 *: TRV1): RVOut =
			rv1.head *: nextAlign.alignValues(rv1.tail)
	}

	given emptyAligned[Outer1, Outer2]: MigrationAlign[EmptyTuple, EmptyTuple, Outer1, EmptyTuple, EmptyTuple, Outer2] with {
		type RVOut = EmptyTuple
		type ROut = EmptyTuple
		def alignComponents(r1: EmptyTuple): EmptyTuple = EmptyTuple
		def alignValues(rv1: EmptyTuple): EmptyTuple = EmptyTuple
	}
}

trait NameMigrationAlign[RV1, R1, Outer1, RV2, R2, Outer2] extends Align[RV1, R1, Outer1, RV2, R2, Outer2]

trait NameMigrationAligns2 {
	given backupAlign[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO](
		using
		backup: MigrationAlign.Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO],
	): NameMigrationAlign[RV1, R1, Outer1, RV2, R2, Outer2] with {
		type ROut = RO
		type RVOut = RVO
		def alignComponents(r1: R1): RO = backup.alignComponents(r1)
		def alignValues(rv1: RV1): RVO = backup.alignValues(rv1)
	}
}

trait NameMigrationAligns1 extends NameMigrationAligns2 {
	given unaligned[HRV1, TRV1 <: Tuple, HR1, TR1 <: Tuple, Outer1, RV2 <: Tuple, R2 <: Tuple, Outer2, RVO <: Tuple, RO <: Tuple](
		using
		next: NameMigrationAlign.Aux[Append[TRV1, HRV1], Append[TR1, HR1], Outer1, RV2, R2, Outer2, RVO, RO],
	): NameMigrationAlign[HRV1 *: TRV1, HR1 *: TR1, Outer1, RV2, R2, Outer2] with {
		type ROut = RO
		type RVOut = RVO

		def alignComponents(r1: HR1 *: TR1): RO =
			next.alignComponents((r1.tail ++ (r1.head *: EmptyTuple)).asInstanceOf[Append[TR1, HR1]])
		def alignValues(rv1: HRV1 *: TRV1): RVO =
			next.alignValues((rv1.tail ++ (rv1.head *: EmptyTuple)).asInstanceOf[Append[TRV1, HRV1]])
	}
}

object NameMigrationAlign extends NameMigrationAligns1 {
	type Aux[RV1, R1, Outer1, RV2, R2, Outer2, RVO, RO] = NameMigrationAlign[RV1, R1, Outer1, RV2, R2, Outer2] { type RVOut = RVO; type ROut = RO }

	given aligned[Outer1, Outer2, HRV1, TRV1 <: Tuple, N <: String & Singleton, HR1 <: Component.Tpe[HRV1] & Component.Named[N], TR1 <: Tuple, HRV2, TRV2 <: Tuple, HR2 <: Component.Tpe[HRV2] & Component.Named[N], TR2 <: Tuple, NextRV <: Tuple, NextR <: Tuple] (
		using
		migr: ShapeMigration.Aux[HRV1, HR1, Outer1, HRV2, HR2, Outer2],
		next: NameMigrationAlign.Aux[TRV1, TR1, Outer1, TRV2, TR2, Outer2, NextRV, NextR],
	): NameMigrationAlign[HRV1 *: TRV1, HR1 *: TR1, Outer1, HRV2 *: TRV2, HR2 *: TR2, Outer2] with {
		type ROut = HR1 *: NextR
		type RVOut = HRV1 *: NextRV

		def alignComponents(r1: HR1 *: TR1): ROut =
			r1.head *: next.alignComponents(r1.tail)
		def alignValues(rv1: HRV1 *: TRV1): RVOut =
			rv1.head *: next.alignValues(rv1.tail)
	}

	given emptyAligned[Outer1, Outer2]: NameMigrationAlign[EmptyTuple, EmptyTuple, Outer1, EmptyTuple, EmptyTuple, Outer2] with {
		type RVOut = EmptyTuple
		type ROut = EmptyTuple
		def alignComponents(r1: EmptyTuple): EmptyTuple = EmptyTuple
		def alignValues(rv1: EmptyTuple): EmptyTuple = EmptyTuple
	}
}
