package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.product.field.{Field, FieldName}

trait Isomorphism[ A, B ] {
    def convertForward(a: A): B
    def convertBackward(b: B): A
}

object Isomorphism {
    given isomorphismFromSchemas[ A, AS, B, BS ](
        using
        aSch: Schema.Aux[A, AS],
        bSch: Schema.Aux[B, BS],
        si : ShapeIsomorphism.Aux[ A, AS, B, BS ],
    ) : Isomorphism[ A, B ] with {
        def convertForward(a: A): B = si.convertForward(a, aSch.shape, bSch.shape)
        def convertBackward(b: B): A = si.convertBackward(b, aSch.shape, bSch.shape)
    }
}

trait ShapeIsomorphism[ A, B ] {
    type AShape
    type BShape

    def convertForward(a: A, aShape: AShape, bShape: BShape): B
    def convertBackward(b: B, AShape: AShape, bShape: BShape): A
}

object ShapeIsomorphism {
    type Aux[A, AS, B, BS] = ShapeIsomorphism[A, B] { type AShape = AS; type BShape = BS }

    given identityIsomorphism[A, AS] : ShapeIsomorphism[A, A] with {
        type AShape = AS
        type BShape = AS

        def convertForward(a: A, aShape: AShape, bShape: AShape): A = a
        def convertBackward(b: A, aShape: AShape, bShape: AShape): A = b
    }

    given singletonIsomorphism[A <: Singleton, B <: Singleton, AN <: TypeName, BN <: TypeName ] : ShapeIsomorphism[ A, B ] with {
        type AShape = SingletonShape[ A, AN ]
        type BShape = SingletonShape[ B, BN ]

        def convertForward(
            a: A, aShape: AShape, bShape: BShape
        ): B = bShape.value

        def convertBackward(
            b: B, aShape: AShape, bShape: BShape
        ): A = aShape.value
    }

    given productIsomorphismWithAF[A, AR <: Tuple, ARV <: Tuple, AAF, AAFS, AAFE, AC, B, BR <: Tuple, BRV <: Tuple, BAF, BAFS, BAFE, BC](
        using
        fieldsIso : ShapeIsomorphism.Aux[ ARV, AR, BRV, BR ],
        ad : ProductDeconstructor.Aux[ A, (AAFE, AR), (Map[String, AAF], ARV) ],
        bd : ProductDeconstructor.Aux[ B, (BAFE, BR), (Map[String, BAF], BRV) ],
        ac : ProductConstructor[ AC, ARV, AAF, A ],
        bc : ProductConstructor[ BC, BRV, BAF, B ],
        afIso : ShapeIsomorphism.Aux[ AAF, AAFS, BAF, BAFS ],
    ) : ShapeIsomorphism[A, B] with {
        type AShape = ProductShape[A, AR, ARV, AAF, AAFS, AAFE, AC]
        type BShape = ProductShape[B, BR, BRV, BAF, BAFS, BAFE, BC]

        def convertForward(a: A, aShape: AShape, bShape: BShape): B =
            val (aaf, arv) = ad.deconstruct(a, (aShape.afExtractor, aShape.fieldDescriptions))
            val brv = fieldsIso.convertForward(arv, aShape.fieldDescriptions, bShape.fieldDescriptions)
            val baf = aaf.map {
                case (key, aafValue) => key -> afIso.convertForward(aafValue, aShape.additionalFieldsSchema.shape, bShape.additionalFieldsSchema.shape)
            }
            bc.construct(bShape.constructor)(brv, baf)

        def convertBackward(b: B, aShape: AShape, bShape: BShape): A =
            val (baf, brv) = bd.deconstruct(b, (bShape.afExtractor, bShape.fieldDescriptions))
            val arv = fieldsIso.convertBackward(brv, aShape.fieldDescriptions, bShape.fieldDescriptions)
            val aaf = baf.map {
                case (key, bafValue) => key -> afIso.convertBackward(bafValue, aShape.additionalFieldsSchema.shape, bShape.additionalFieldsSchema.shape)
            }
            ac.construct(aShape.constructor)(arv, aaf)
    }

    given productIsomorphismNoAF[A, AR <: Tuple, ARV <: Tuple, AC, B, BR <: Tuple, BRV <: Tuple, BC](
        using
        fieldsIso : ShapeIsomorphism.Aux[ ARV, AR, BRV, BR ],
        ad : ProductDeconstructor.Aux[ A, (Unit, AR), ARV ],
        bd : ProductDeconstructor.Aux[ B, (Unit, BR), BRV ],
        ac : ProductConstructor[ AC, ARV, Nothing, A ],
        bc : ProductConstructor[ BC, BRV, Nothing, B ],
    ) : ShapeIsomorphism[A, B] with {
        type AShape = ProductShape[A, AR, ARV, Nothing, Unit, Unit, AC]
        type BShape = ProductShape[B, BR, BRV, Nothing, Unit, Unit, BC]

        def convertForward(a: A, aShape: AShape, bShape: BShape): B =
            val arv = ad.deconstruct(a, (aShape.afExtractor, aShape.fieldDescriptions))
            val brv = fieldsIso.convertForward(arv, aShape.fieldDescriptions, bShape.fieldDescriptions)
            bc.construct(bShape.constructor)(brv, Map.empty)

        def convertBackward(b: B, aShape: AShape, bShape: BShape): A =
            val brv = bd.deconstruct(b, (bShape.afExtractor, bShape.fieldDescriptions))
            val arv = fieldsIso.convertBackward(brv, aShape.fieldDescriptions, bShape.fieldDescriptions)
            ac.construct(aShape.constructor)(arv, Map.empty)
    }

    given fieldIsomorphism[AT, A, AN <: FieldName, AS, BT, B, BN <: FieldName, BS](
        using
        iso : ShapeIsomorphism.Aux[ A, AS, B, BS ],
    ) : ShapeIsomorphism[A, B] with {
        type AShape = Field[ AT, A, AN, AS ]
        type BShape = Field[ BT, B, BN, BS ]

        def convertForward(a : A, aShape : AShape, bShape : BShape): B =
            iso.convertForward(a, aShape.schema.shape, bShape.schema.shape)

        def convertBackward(b : B, aShape : AShape, bShape: BShape): A =
            iso.convertBackward(b, aShape.schema.shape, bShape.schema.shape)
    }

    given nonEmptyFieldsTupleIso[ AHead, ATail <: Tuple, BHead, BTail <: Tuple, ARHead, ARTail <: Tuple, BRHead, BRTail <: Tuple ](
        using
        headIso : ShapeIsomorphism.Aux[ AHead, ARHead, BHead, BRHead ],
        tailIso : ShapeIsomorphism.Aux[ ATail, ARTail, BTail, BRTail ],
    ) : ShapeIsomorphism[ AHead *: ATail, BHead *: BTail ] with {
        type AShape = ARHead *: ARTail
        type BShape = BRHead *: BRTail

        def convertForward(a : AHead *: ATail, aShape : AShape, bShape: BShape): BHead *: BTail =
            val bHead = headIso.convertForward(a.head, aShape.head, bShape.head)
            val bTail = tailIso.convertForward(a.tail, aShape.tail, bShape.tail)
            bHead *: bTail

        def convertBackward(b : BHead *: BTail, aShape : AShape, bShape: BShape): AHead *: ATail =
            val aHead = headIso.convertBackward(b.head, aShape.head, bShape.head)
            val aTail = tailIso.convertBackward(b.tail, aShape.tail, bShape.tail)
            aHead *: aTail
    }

    given coproductIsomorphism[A, AR <: Tuple, ARV <: Tuple, AD, ADN, B, BR <: Tuple, BRV <: Tuple, BD, BDN](
        using
        subtypesIso : CoproductShapeIsomorphism.Aux[A, AR, B, BR],
    ) : ShapeIsomorphism[A, B] with {
        type AShape = CoproductShape[A, AR, ARV, AD, ADN]
        type BShape = CoproductShape[B, BR, BRV, BD, BDN]

        def convertForward(a: A, aShape: AShape, bShape: BShape): B =
            subtypesIso.convertForward(a, aShape.subtypeDescriptions, bShape.subtypeDescriptions)

        def convertBackward(b: B, aShape: AShape, bShape: BShape): A =
            subtypesIso.convertBackward(b, aShape.subtypeDescriptions, bShape.subtypeDescriptions)
    }

}

trait CoproductShapeIsomorphism[ A, B ] extends ShapeIsomorphism[ A, B ] { type AShape <: Tuple; type BShape <: Tuple }

object CoproductShapeIsomorphism {
    type Aux[ A, AS <: Tuple, B, BS <: Tuple ] = CoproductShapeIsomorphism[A, B] { type AShape = AS; type BShape = BS }

    given coproductNonEmptySubtypesTupleIso[A, AS, AD, ADN, ADV, AN <: TypeName, ASS, ATail <: Tuple, B, BS, BD, BDN, BDV, BN <: TypeName, BSS, BTail <: Tuple ](
        using
        stIso : ShapeIsomorphism.Aux[AS, ASS, BS, BSS],
        tailIso : CoproductShapeIsomorphism.Aux[A, ATail, B, BTail],
    ): CoproductShapeIsomorphism[ A, B ] with {
        type AShape = Subtype[ A, AS, AD, ADN, ADV, AN, ASS ] *: ATail
        type BShape = Subtype[ B, BS, BD, BDN, BDV, BN, BSS ] *: BTail

        def convertForward(a: A, aShape: AShape, bShape: BShape): B =
            aShape
              .head
              .fromSuper(a)
              .map(as => {
                  val bs = stIso.convertForward(as, aShape.head.schema.shape, bShape.head.schema.shape)
                  bShape.head.toSuper(bs)
              })
              .getOrElse(tailIso.convertForward(a, aShape.tail, bShape.tail))

        def convertBackward(b: B, aShape: AShape, bShape: BShape): A =
            bShape
              .head
              .fromSuper(b)
              .map(bs => {
                  val as = stIso.convertBackward(bs, aShape.head.schema.shape, bShape.head.schema.shape)
                  aShape.head.toSuper(as)
              })
              .getOrElse(tailIso.convertBackward(b, aShape.tail, bShape.tail))
    }

    given emptyCoproductIso[A, B] : CoproductShapeIsomorphism[ A, B ] with {
        type AShape = EmptyTuple
        type BShape = EmptyTuple

        def convertForward(a: A, aShape: AShape, bShape: BShape): B = throw new Exception("No subtypes found")
        def convertBackward(b: B, aShape: AShape, bShape: BShape): A = throw new Exception("No subtypes found")
    }
}

trait IsomorphismDsl {
    extension [A](value : A)
        def convert[B](using iso : Isomorphism[A, B]): B = iso.convertForward(value)
}

object IsomorphismDsl extends IsomorphismDsl
