package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.selector.{ComponentRetriever, FieldSelector, SubTypeSelector}
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, SubtypeReplacer, SubtypeRetriever, TypeName}
import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldRetriever, LazyField}

import scala.util.Try

trait Lens[T, S, Selector] {
    type Inner
    type Modifier = Inner => Inner
    type Out

    def outToInnerOption(out: Out): Option[Inner]
    def retrieve(value: T, sch : Schema.Aux[T, S]): Out
    def modify(value: T, sch : Schema.Aux[T, S], modifier: Modifier): T
}

trait Lenses1 {
    given tupleSelectorLensNoOpt[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, SelHead, SelTail <: Tuple, N <: FieldName, HI, HIS, TI, TO ](
        using
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ], SelHead, Field[ T, HI, N, HIS ] ],
        hLens : Lens.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C], SelHead, HI, HI ],
        tLens : Lens.Aux[ HI, HIS, SelTail, TI, TO ],
    ) : Lens[ T, ProductShape[T, R, RV, AF, AFS, AFE, C], SelHead *: SelTail ] with {
        type Inner = TI
        type Out = TO

        override def outToInnerOption(
            out: TO,
        ): Option[ TI ] = tLens.outToInnerOption(out)

        def retrieve( value: T, sch: Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ] ): TO =
            val hi : HI = hLens.retrieve( value, sch )
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            tLens.retrieve( hi, hiSch )

        def modify( value: T, sch: Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ], modifier: TI => TI ): T =
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            hLens.modify(value, sch, (hi : HI) => {
                val to : TO = retrieve(value, sch)
                val tiOpt : Option[ TI ] = outToInnerOption(to)
                tiOpt match
                    case None => hi
                    case Some( innerValue ) =>
                        tLens.modify( hi, hiSch, modifier )
            })
    }

    given tupleSelectorLensNoOptLazy[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, SelHead, SelTail <: Tuple, N <: FieldName, HI, HIS, TI, TO ](
        using
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ], SelHead, LazyField[ T, HI, N ] ],
        hiSch : Schema.Aux[ HI, HIS ],
        hLens : Lens.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C], SelHead, HI, HI ],
        tLens : Lens.Aux[ HI, HIS, SelTail, TI, TO ],
    ) : Lens[ T, ProductShape[T, R, RV, AF, AFS, AFE, C], SelHead *: SelTail ] with {
        type Inner = TI
        type Out = TO

        override def outToInnerOption(
            out: TO,
        ): Option[ TI ] = tLens.outToInnerOption(out)

        def retrieve( value: T, sch: Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ] ): TO =
            val hi : HI = hLens.retrieve( value, sch )
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            tLens.retrieve( hi, hiSch )

        def modify( value: T, sch: Schema.Aux[ T, ProductShape[T, R, RV, AF, AFS, AFE, C] ], modifier: TI => TI ): T =
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            hLens.modify(value, sch, (hi : HI) => {
                val to : TO = retrieve(value, sch)
                val tiOpt : Option[ TI ] = outToInnerOption(to)
                tiOpt match
                    case None => hi
                    case Some( innerValue ) =>
                        tLens.modify( hi, hiSch, modifier )
            })
    }
}

object Lens extends Lenses1 {
    type Aux[ T, S, Selector, I, O ] = Lens[ T, S, Selector ] { type Inner = I; type Out = O }

    given subtypeLens[T, Sel <: Singleton, N <: TypeName, R <: Tuple, STT, DV,  ST <: Subtype.OrLazy[ T, STT, D, DN, DV, N ], RV <: Tuple, D, DN](
        using
        strt : SubtypeRetriever.Aux[Sel, R, ST],
    ): Lens[T, CoproductShape[T, R, RV, D, DN], SubTypeSelector[Sel]] with {
        type Inner = STT
        type Out = Option[STT]

        override def outToInnerOption( out: Option[STT] ): Option[ STT ] = out

        def retrieve( value: T, sch : Schema.Aux[T, CoproductShape[T, R, RV, D, DN]] ): Option[STT] =
            val subtype : ST = strt.retrieve(sch.shape.subtypeDescriptions)
            subtype.fromSuper( value )

        def modify( value: T, sch : Schema.Aux[T, CoproductShape[T, R, RV, D, DN]], modifier: STT => STT ): T =
            val subtype : ST = strt.retrieve(sch.shape.subtypeDescriptions)
            subtype.fromSuper( value ) match
                case Some( value ) => subtype.toSuper( modifier(value) )
                case None => value
    }

    given fieldLensNoaf[T, Sel <: Singleton, N <: FieldName, R <: Tuple, F, Fld <: Field.OrLazy[T, F, N], RV <: Tuple, C](
        using
        frt : FieldRetriever.Aux[Sel, R, Fld],
        pdc : ProductDeconstructor.Aux[ T, R, RV ],
        fvr : FieldValueReplacer.Aux[Sel, R, RV, F],
        pcn : ProductConstructor[ C, RV, Nothing, T ],
    ) : Lens[T, ProductShape[T, R, RV, Nothing, Unit, Unit, C], FieldSelector[Sel]] with {
        type Inner = F
        type Out = F

        override def outToInnerOption(
            out: F
        ): Option[ F ] = Some( out )

        def retrieve(value: T, sch: Schema.Aux[T, ProductShape[T, R, RV, Nothing, Unit, Unit, C]]): F =
            val field : Fld = frt.retrieve(sch.shape.fieldDescriptions)
            field.extractor(value)

        def modify(value: T, sch: Schema.Aux[T, ProductShape[T, R, RV, Nothing, Unit, Unit, C]], modifier: F => F): T =
            val field : Fld = frt.retrieve(sch.shape.fieldDescriptions)
            val inner = retrieve(value, sch)
            val newInner = modifier(inner)
            val rv = pdc.deconstruct(value, sch.shape.fieldDescriptions)
            val newRV = fvr.replace(sch.shape.fieldDescriptions, rv, newInner)
            pcn.construct(sch.shape.construct)(newRV)
    }

    given fieldLensAf[T, Sel <: Singleton, N <: FieldName, R <: Tuple, F, Fld <: Field.OrLazy[T, F, N], RV <: Tuple, AF, AFS, AFE, C](
        using
        frt : FieldRetriever.Aux[Sel, R, Fld],
        pdc : ProductDeconstructor.Aux[ T, (AFE, R), (Map[String, AF], RV) ],
        fvr : FieldValueReplacer.Aux[Sel, R, RV, F],
        pcn : ProductConstructor[ C, RV, AF, T ],
    ) : Lens[T, ProductShape[T, R, RV, AF, AFS, AFE, C], FieldSelector[Sel]] with {
        type Inner = F
        type Out = F

        override def outToInnerOption(
            out: F
        ): Option[ F ] = Some( out )

        def retrieve(value: T, sch : Schema.Aux[T, ProductShape[T, R, RV, AF, AFS, AFE, C]]): F =
            val field : Fld = frt.retrieve(sch.shape.fieldDescriptions)
            field.extractor(value)

        def modify(value: T, sch : Schema.Aux[T, ProductShape[T, R, RV, AF, AFS, AFE, C]], modifier: F => F): T =
            val inner = retrieve(value, sch)
            val newInner = modifier(inner)
            val (af, rv) = pdc.deconstruct(value, (sch.shape.afExtractor, sch.shape.fieldDescriptions))
            val newRV = fvr.replace(sch.shape.fieldDescriptions, rv, newInner)
            pcn.construct(sch.shape.construct)(newRV, af)
    }

//    given lastSelLense[ T, S, Sel, I, O ](
//        using
//        selLens : Lens.Aux[ T, S, Sel, I, O ]
//    ) : Lens[ T, S, Sel *: EmptyTuple ] with {
//        type Inner = I
//        type Out = O
//
//        def retrieve( value: T, sch: Schema.Aux[T, S] ): O = selLens.retrieve(value, sch)
//        def modify( value: T, sch: Schema.Aux[T, S], modifier: I => I ): T = selLens.modify(value, sch, modifier)
//    }

    given lastSelLense[ T, S ] : Lens[ T, S, EmptyTuple ] with {
        type Inner = T
        type Out = T

        override def outToInnerOption(out: T): Option[ T ] = Some( out )
        def retrieve( value: T, sch: Schema.Aux[T, S] ): T = value
        def modify( value: T, sch: Schema.Aux[T, S], modifier: T => T ): T = modifier(value)
    }

    given tupleSelectorLensOpt[ T, R <: Tuple, RV <: Tuple, D, DN, SelHead, SelTail <: Tuple, DV, N <: TypeName, HI, HIS, I, O ](
        using
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ], SelHead, Subtype[ T, HI, D, DN, DV, N, HIS ] ],
        hLens : Lens.Aux[ T, CoproductShape[T, R, RV, D, DN], SelHead, HI, Option[HI] ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, CoproductShape[T, R, RV, D, DN], SelHead *: SelTail ] with {
        type Inner = I
        type Out = Option[O]

        override def outToInnerOption(
            out: Option[O]
        ): Option[ I ] = out.flatMap(o => tLens.outToInnerOption(o))

        def retrieve( value: T, sch: Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ] ): Option[O] =
            val hiOpt : Option[HI] = hLens.retrieve( value, sch )
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            hiOpt.map(hi => tLens.retrieve( hi, hiSch ))

        def modify( value: T, sch: Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ], modifier: I => I ): T =
            val hiSch : Schema.Aux[ HI, HIS ] = cRt.retrieve( sch ).schema
            hLens.modify(value, sch, (hi : HI) => {
                val oOpt : Option[O] = retrieve(value, sch)
                val iOpt : Option[ I ] = outToInnerOption(oOpt)
                iOpt match
                    case None => hi
                    case Some( innerValue ) =>
                        tLens.modify( hi, hiSch, modifier )
            })
    }

    given tupleSelectorLensOptLazy[ T, R <: Tuple, RV <: Tuple, D, DN, SelHead, SelTail <: Tuple, DV, N <: TypeName, HI, HIS, I, O ](
        using
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ], SelHead, LazySubtype[ T, HI, D, DN, DV, N ] ],
        hiSch : Schema.Aux[ HI, HIS ],
        hLens : Lens.Aux[ T, CoproductShape[T, R, RV, D, DN], SelHead, HI, Option[HI] ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, CoproductShape[T, R, RV, D, DN], SelHead *: SelTail ] with {
        type Inner = I
        type Out = Option[O]

        override def outToInnerOption(
            out: Option[O]
        ): Option[ I ] = out.flatMap(o => tLens.outToInnerOption(o))

        def retrieve( value: T, sch: Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ] ): Option[O] =
            val hiOpt : Option[HI] = hLens.retrieve( value, sch )
            hiOpt.map(hi => tLens.retrieve( hi, hiSch ))

        def modify( value: T, sch: Schema.Aux[ T, CoproductShape[T, R, RV, D, DN] ], modifier: I => I ): T =
            hLens.modify(value, sch, (hi : HI) => {
                val oOpt : Option[O] = retrieve(value, sch)
                val iOpt : Option[ I ] = outToInnerOption(oOpt)
                iOpt match
                    case None => hi
                    case Some( innerValue ) =>
                        tLens.modify( hi, hiSch, modifier )
            })
    }

}

type OptionOf[A] = [OA] =>> OptionalValue[A, OA]

object OptionOf {
    def apply[A, B](using ov: OptionalValue[A, B]) : OptionalValue[A, B] = ov
}

trait OptionalValue[A, OA] {
    def toOption(oa: OA): Option[A]
    def fromOption(oa : Option[A]): OA
}

trait OptionalValues2 {
    given nestedOption[A, B](
        using
        innerOV: OptionalValue[A, B],
    ) : OptionalValue[A, Option[B]] with {
        override def toOption( oa: Option[ B ] ): Option[ A ] =
            oa.flatMap( b => innerOV.toOption(b) )

        override def fromOption( oa: Option[ A ] ): Option[ B ] =
            Try( innerOV.fromOption(oa) ).toOption
    }
}

trait OptionalValues1 extends OptionalValues2 {
    given isOption[A] : OptionalValue[A, Option[A]] with {
        override def toOption( oa: Option[ A ] ): Option[ A ] = oa
        override def fromOption( oa: Option[ A ] ): Option[ A ] = oa
    }
}

object OptionalValue extends OptionalValues1 {
    given identical[A] : OptionalValue[A, A] with {
        override def toOption( oa: A ): Option[ A ] = Some(oa)
        override def fromOption( oa: Option[ A ] ): A = oa.get
    }
}
