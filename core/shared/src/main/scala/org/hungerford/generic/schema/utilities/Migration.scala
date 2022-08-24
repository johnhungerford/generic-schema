package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.{Schema, SchemaExtractor, SchemaProvider}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.types.{Filter, Of, OfShape}
import org.hungerford.generic.schema.Component
import org.hungerford.generic.schema.Component.WithSchema

trait Migration[A, B] {
	def migrate(from: A): B
}

object Migration {
	given fromShapeMigration[A, B, AShape, BShape](
		using
		sA: Schema.Aux[A, AShape],
		sB: Schema.Aux[B, BShape],
		sm: ShapeMigration.Aux[A, AShape, A, B, BShape, B],
	) : Migration[A, B] with {
		def migrate(from: A): B = sm.migrate(from, sA.shape, sB.shape)
	}
}

trait ShapeMigration[A, OuterA, B, OuterB] {
	type AShape
	type BShape

	def migrate(a: A, aShape: AShape, bShape: BShape): B
}

object ShapeMigration {
	type Aux[A, AS, OuterA, B, BS, OuterB] = ShapeMigration[A, OuterA, B, OuterB] {type AShape = AS; type BShape = BS}

	given identityMigration[A, AS, OuterA, OuterB]: ShapeMigration[A, OuterA, A, OuterB] with {
		type AShape = AS
		type BShape = AS

		def migrate(a: A, aShape: AShape, bShape: AShape): A = a
	}

	given singletonMigration[A <: Singleton, OA, B <: Singleton, OB, AN <: TypeName, BN <: TypeName]: ShapeMigration[A, OA, B, OB] with {
		type AShape = SingletonShape[A, AN]
		type BShape = SingletonShape[B, BN]

		def migrate(
			a: A, aShape: AShape, bShape: BShape
		): B = bShape.value
	}

	given componentMigration[T1, C1 <: Component.Tpe[T1], Outer1, T2, C2 <: Component.Tpe[T2], Outer2, S1, S2](
		using
		ws1 : WithSchema.Aux[C1, Outer1, T1, S1],
		ws2 : WithSchema.Aux[C2, Outer2, T2, S2],
		sm: ShapeMigration.Aux[T1, S1, Outer1, T2, S2, Outer2],
	): ShapeMigration[T1, Outer1, T2, Outer2] with {
		type AShape = C1
		type BShape = C2

		def migrate(a: T1, aShape: C1, bShape: C2): T2 =
			val s1 = ws1.schemaOf(aShape).shape
			val s2 = ws2.schemaOf(bShape).shape
			sm.migrate(a, s1, s2)
	}

	given productMigration[A, OA, ARV <: Tuple, AR <: Tuple, AAF, AAFS, AAFE, AC, B, OB, BR <: Tuple, BRV <: Tuple, BAF, BAFS, BAFE, BC] (
		using
		fieldsIso: ShapeMigration.Aux[ARV, AR, OA, BRV, BR, OB],
		ad: ProductDeconstructor.Aux[A, (AAFE, AR), (Map[String, AAF], ARV)],
		bc: ProductConstructor[BC, BRV, BAF, B],
		afIso: ShapeMigration.Aux[AAF, AAFS, OA, BAF, BAFS, OB],
	): ShapeMigration[A, OA, B, OB] with {
		type AShape = ProductShape[A, AR, ARV, AAF, AAFS, AAFE, AC]
		type BShape = ProductShape[B, BR, BRV, BAF, BAFS, BAFE, BC]

		def migrate(a: A, aShape: AShape, bShape: BShape): B =
			val (aaf, arv) = ad.deconstruct(a, (aShape.afExtractor, aShape.fieldDescriptions))
			val brv = fieldsIso
			  .migrate(arv, aShape.fieldDescriptions, bShape.fieldDescriptions)
			val baf = aaf.map {
				case (key, aafValue) => key -> (afIso
				  .migrate(
					  aafValue,
					  aShape.additionalFieldsSchema.shape,
					  bShape.additionalFieldsSchema.shape,
				  ))
			}
			bc.construct(bShape.constructor)(brv, baf)
	}

	given productMigrationNoAF[A, OA, AR <: Tuple, ARV <: Tuple, AC, B, OB, BR <: Tuple, BRV <: Tuple, BC] (
		using
		fieldsIso: ShapeMigration.Aux[ARV, AR, OA, BRV, BR, OB],
		ad: ProductDeconstructor.Aux[A, (Unit, AR), ARV],
		bc: ProductConstructor[BC, BRV, Nothing, B],
	): ShapeMigration[A, OA, B, OB] with {
		type AShape = ProductShape[A, AR, ARV, Nothing, Unit, Unit, AC]
		type BShape = ProductShape[B, BR, BRV, Nothing, Unit, Unit, BC]

		def migrate(a: A, aShape: AShape, bShape: BShape): B =
			val arv = ad.deconstruct(a, (aShape.afExtractor, aShape.fieldDescriptions))
			val brv = fieldsIso
			  .migrate(arv, aShape.fieldDescriptions, bShape.fieldDescriptions)
			bc.construct(bShape.constructor)(brv, Map.empty)
	}

//	given fieldMigration[AT, A, AN <: FieldName, AS, OA, BT, B, BN <: FieldName, BS, OB] (
//		using
//		iso: ShapeMigration.Aux[A, AS, OA, B, BS, OB],
//	): ShapeMigration[A, OA, B, OB] with {
//		type AShape = Field[AT, A, AN, AS]
//		type BShape = Field[BT, B, BN, BS]
//
//		def migrate(a: A, aShape: AShape, bShape: BShape): B =
//			iso.migrate(a, aShape.schema.shape, bShape.schema.shape)
//	}
//
//	given lazyFieldMigration[OA, OAS, AT, A, AN <: FieldName, AS, BT, OB, OBS, B, BN <: FieldName, BS] (
//		using
//		oaSch: Schema.Aux[OA, OAS],
//		aSchExtr: SchemaExtractor.Aux[A, Schema.Aux[OA, OAS], AS],
//		obSch: Schema.Aux[OB, OBS],
//		bSchExtr: SchemaExtractor.Aux[B, Schema.Aux[OB, OBS], BS],
//		iso: ShapeMigration.Aux[A, AS, OA, B, BS, OB],
//	): ShapeMigration[A, OA, B, OB] with {
//		type AShape = LazyField[AT, A, AN]
//		type BShape = LazyField[BT, B, BN]
//
//		lazy val afShape = aSchExtr.extract(oaSch).shape
//		lazy val bfShape = bSchExtr.extract(obSch).shape
//
//		def migrate(a: A, aShape: AShape, bShape: BShape): B =
//			iso.migrate(a, afShape, bfShape)
//	}

	given nonEmptyFieldsTupleIso[AHead, OA, ATail <: Tuple, BHead, BTail <: Tuple, ARHead, ARTail <: Tuple, BRHead, OB, BRTail <: Tuple] (
		using
		headIso: => ShapeMigration.Aux[AHead, ARHead, OA, BHead, BRHead, OB],
		tailIso: => ShapeMigration.Aux[ATail, ARTail, OA, BTail, BRTail, OB],
	): ShapeMigration[AHead *: ATail, OA, BHead *: BTail, OB] with {
		type AShape = ARHead *: ARTail
		type BShape = BRHead *: BRTail

		def migrate(a: AHead *: ATail, aShape: AShape, bShape: BShape): BHead *: BTail =
			val bHead = headIso.migrate(a.head, aShape.head, bShape.head)
			val bTail = tailIso.migrate(a.tail, aShape.tail, bShape.tail)
			bHead *: bTail
	}

	given coproductMigration[A, OA, AR <: Tuple, ARV <: Tuple, AD, ADN, B, OB, BR <: Tuple, BRV <: Tuple, BD, BDN] (
		using
		subtypesIso: CoproductShapeMigration.Aux[A, AR, OA, B, BR, OB],
	): ShapeMigration[A, OA, B, OB] with {
		type AShape = CoproductShape[A, AR, ARV, AD, ADN]
		type BShape = CoproductShape[B, BR, BRV, BD, BDN]

		def migrate(a: A, aShape: AShape, bShape: BShape): B =
			subtypesIso.migrate(a, aShape.subtypeDescriptions, bShape.subtypeDescriptions)
	}

}

trait CoproductShapeMigration[A, OuterA, B, OuterB]
  extends ShapeMigration[A, OuterA, B, OuterB] {
	type AShape <: Tuple;
	type BShape <: Tuple
}

object CoproductShapeMigration {
	type Aux[A, AS <: Tuple, OA, B, BS <: Tuple, OB] = CoproductShapeMigration[A, OA, B, OB] {type AShape = AS; type BShape = BS}

	given coproductNonEmptySubtypesTupleIso[A, OA, AST, AC <: Subtype.SubOf[A, AST], ATail <: Tuple, B, OB, BST, BC <: Subtype.SubOf[B, BST], BTail <: Tuple] (
		using
		stIso: ShapeMigration.Aux[AST, AC, OA, BST, BC, OB],
		tailIso: => CoproductShapeMigration.Aux[A, ATail, OA, B, BTail, OB],
	): CoproductShapeMigration[A, OA, B, OB] with {
		type AShape = AC *: ATail
		type BShape = BC *: BTail

		def migrate(a: A, aShape: AShape, bShape: BShape): B =
			aShape
			  .head
			  .fromSuper(a)
			  .map(as => bShape.head.toSuper(stIso.migrate(as, aShape.head, bShape.head)))
			  .getOrElse(tailIso.migrate(a, aShape.tail, bShape.tail))
	}

//	given coproductNonEmptyLazySubtypesTupleIso[A, OA, AS, AD, ADN, ADV, AN <: TypeName, ASS, ATail <: Tuple, B, OB, BS, BD, BDN, BDV, BN <: TypeName, BSS, BTail <: Tuple] (
//		using
//		stIso: ShapeMigration.Aux[AS, Subtype[A, AS, AD, ADN, ADV, AN, ASS], OA, BS, Subtype[B, BS, BD, BDN, BDV, BN, BSS], OB],
//		tailIso: => CoproductShapeMigration.Aux[A, ATail, OA, B, BTail, OB],
//	): CoproductShapeMigration[A, OA, B, OB] with {
//		type AShape = Subtype[A, AS, AD, ADN, ADV, AN, ASS] *: ATail
//		type BShape = Subtype[B, BS, BD, BDN, BDV, BN, BSS] *: BTail
//
//		def migrate(a: A, aShape: AShape, bShape: BShape): B =
//			aShape
//			  .head
//			  .fromSuper(a)
//			  .map(as => bShape.head.toSuper(stIso.migrate(as, aShape.head, bShape.head)))
//			  .getOrElse(tailIso.migrate(a, aShape.tail, bShape.tail))
//	}
//
//	given coproductNonEmptySubtypesTupleIsoLazy[A, OA, OAS, AS, AD, ADN, ADV, AN <: TypeName, ASS, ATail <: Tuple, B, OB, OBS, BS, BD, BDN, BDV, BN <: TypeName, BSS, BTail <: Tuple] (
//		using
//		oaSch: Schema.Aux[OA, OAS],
//		asSchExtr: SchemaExtractor.Aux[AS, Schema.Aux[OA, OAS], ASS],
//		stIso: ShapeMigration.Aux[AS, ASS, OA, BS, BSS, OB],
//		obSch: Schema.Aux[OB, OBS],
//		bsSchExtr: SchemaExtractor.Aux[BS, Schema.Aux[OB, OBS], BSS],
//		tailIso: => CoproductShapeMigration.Aux[A, ATail, OA, B, BTail, OB],
//	): CoproductShapeMigration[A, OA, B, OB] with {
//		type AShape = LazySubtype[A, AS, AD, ADN, ADV, AN] *: ATail
//		type BShape = LazySubtype[B, BS, BD, BDN, BDV, BN] *: BTail
//
//		lazy val asSchShape = asSchExtr.extract(oaSch).shape
//		lazy val bsSchShape = bsSchExtr.extract(obSch).shape
//
//		def migrate(a: A, aShape: AShape, bShape: BShape): B = {
//			aShape.head.fromSuper(a).map { as =>
//				  val bs = stIso.migrate(as, asSchShape, bsSchShape)
//				  bShape.head.toSuper(bs)
//			}
//			  .getOrElse(tailIso.migrate(a, aShape.tail, bShape.tail))
//		}
//	}

	given emptyCoproductIso[A, OA, B, OB]: CoproductShapeMigration[A, OA, B, OB] with {
		type AShape = EmptyTuple
		type BShape = EmptyTuple

		def migrate(a: A, aShape: AShape, bShape: BShape): B = throw new Exception("No subtypes found")

		def convertBackward(b: B, aShape: AShape, bShape: BShape): A = throw new Exception("No subtypes found")
	}
}

trait MigrationDsl {
	extension[A] (value: A)
		def migrateTo[B](using iso: Migration[A, B]): B = iso.migrate(value)
}

object MigrationDsl extends MigrationDsl
