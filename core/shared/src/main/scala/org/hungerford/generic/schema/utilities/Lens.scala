package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.selector.{ComponentRetriever, FieldSelector, SubTypeSelector}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeReplacer, SubtypeRetriever, TypeName}
import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldRetriever}

import scala.util.Try

trait Lens[T, S, Selector] {
    type Inner
    type Modifier = Inner => Inner
    type Out

    def retrieve(value: T, sch : Schema.Aux[T, S]): Out
    def modify(value: T, sch : Schema.Aux[T, S], modifier: Modifier): T
}

object Lens {
    type Aux[ T, S, Selector, I, O ] = Lens[ T, S, Selector ] { type Inner = I; type Out = O }

    given subtypeLens[T, Sel <: Singleton, N <: TypeName, R <: Tuple, STT, DV,  ST <: Subtype.OrLazy[ T, STT, D, DN, DV, N ], RV <: Tuple, D, DN](
        using
        strt : SubtypeRetriever.Aux[Sel, R, ST],
    ): Lens[T, CoproductShape[T, R, RV, D, DN], SubTypeSelector[Sel]] with {
        type Inner = STT
        type Out = Option[STT]

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

    given tupleSelectorLensNoOpt[ T, S, SelHead, SelTail <: Tuple, HIComponent, HI, HIS, I, O : OptionOf[ I ] ](
        using
        hLens : Lens.Aux[ T, S, SelHead, HI, HI ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, S ], SelHead, HIComponent ],
        sE : SchemaExtractor.Aux[ HI, HIComponent, HIS ],
    ) : Lens[ T, S, SelHead *: SelTail ] with {
        type Inner = I
        type Out = O

        def retrieve( value: T, sch: Schema.Aux[ T, S ] ): O =
            val hi : HI = hLens.retrieve( value, sch )
            val hiSch : Schema.Aux[ HI, HIS ] = sE.extract( cRt.retrieve( sch ) )
            tLens.retrieve( hi, hiSch )

        def modify( value: T, sch: Schema.Aux[ T, S ], modifier: I => I ): T =
            val hiSch : Schema.Aux[ HI, HIS ] = sE.extract( cRt.retrieve( sch ) )
            hLens.modify(value, sch, (hi : HI) => {
                val o : O = retrieve(value, sch)
                val iOpt : Option[ I ] = OptionOf[I, O].toOption( o )
                iOpt match
                    case None => hi
                    case Some( innerValue ) =>
                        tLens.modify( hi, hiSch, modifier )
            })
    }

    given tupleSelectorLensOpt[ T, S, SelHead, SelTail <: Tuple, HIComponent, HI, HIS, I, O : OptionOf[ I ] ](
        using
        hLens : Lens.Aux[ T, S, SelHead, HI, Option[HI] ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
        cRt : ComponentRetriever.Aux[ Schema.Aux[ T, S ], SelHead, HIComponent ],
        sE : SchemaExtractor.Aux[ HI, HIComponent, HIS ],
    ) : Lens[ T, S, SelHead *: SelTail ] with {
        type Inner = I
        type Out = Option[O]

        def retrieve( value: T, sch: Schema.Aux[ T, S ] ): Option[O] =
            val hiOpt : Option[HI] = hLens.retrieve( value, sch )
            val hiSch : Schema.Aux[ HI, HIS ] = sE.extract( cRt.retrieve( sch ) )
            hiOpt.map(hi => tLens.retrieve( hi, hiSch ))

        def modify( value: T, sch: Schema.Aux[ T, S ], modifier: I => I ): T =
            val hiSch : Schema.Aux[ HI, HIS ] = sE.extract( cRt.retrieve( sch ) )
            hLens.modify(value, sch, (hi : HI) => {
                val oOpt : Option[O] = retrieve(value, sch)
                val iOpt : Option[ I ] = oOpt.flatMap(o => OptionOf[I, O].toOption( o ))
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

object OptionalValue {

    given identical[A] : OptionalValue[A, A] with {
        override def toOption( oa: A ): Option[ A ] = Some(oa)
        override def fromOption( oa: Option[ A ] ): A = oa.get
    }

    given isOption[A] : OptionalValue[A, Option[A]] with {
        override def toOption( oa: Option[ A ] ): Option[ A ] = oa
        override def fromOption( oa: Option[ A ] ): Option[ A ] = oa
    }

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
