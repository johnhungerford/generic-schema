package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.selector.{AmbigSelector, ComponentRetriever, FieldSelector, Selector, SelectorDsl, SubTypeSelector}
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, SubtypeReplacer, SubtypeRetriever, TypeName}
import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldRetriever, LazyField}

import scala.util.Try

trait Lens[T, Component, Selector] {
    type Inner
    type Modifier = Inner => Inner
    type Out

    def retrieve(value: T, component: Component): Out
    def modify(value: T, component : Component, modifier: Modifier): T
}

object Lens {
    type Aux[ T, S, Selector, I, O ] = Lens[ T, S, Selector ] { type Inner = I; type Out = O }

    given ambigSubtypeSelLens[ T, Component, Sel, I, O ](
        using
        stl : Lens.Aux[ T, Component, SubTypeSelector[ Sel ], I, O ],
    ) : Lens[T, Component, AmbigSelector[ Sel ]] with {
        type Inner = I
        type Out = O

        def retrieve( value: T, component: Component ): O = stl.retrieve(value, component)
        def modify(value: T, component: Component, modifier : I => I): T = stl.modify(value, component, modifier)
    }

    given ambigFieldSelLens[ T, Component, Sel, I, O ](
        using
        stl : Lens.Aux[ T, Component, FieldSelector[ Sel ], I, O ],
    ) : Lens[T, Component, AmbigSelector[ Sel ]] with {
        type Inner = I
        type Out = O

        def retrieve( value: T, component: Component ): O = stl.retrieve(value, component)
        def modify(value: T, component: Component, modifier : I => I): T = stl.modify(value, component, modifier)
    }

    given subtypeLens[T, Sel <: Singleton, N <: TypeName, R <: Tuple, STT, DV,  ST <: Subtype.OrLazy[ T, STT, D, DN, DV, N ], RV <: Tuple, D, DN](
        using
        strt : SubtypeRetriever.Aux[Sel, R, ST],
    ): Lens[T, CoproductShape[T, R, RV, D, DN], SubTypeSelector[Sel]] with {
        type Inner = STT
        type Out = Option[STT]

        def retrieve( value: T, component : CoproductShape[T, R, RV, D, DN] ): Option[STT] =
            val subtype : ST = strt.retrieve(component.subtypeDescriptions)
            subtype.fromSuper( value )

        def modify( value: T, component : CoproductShape[T, R, RV, D, DN], modifier: STT => STT ): T =
            val subtype : ST = strt.retrieve(component.subtypeDescriptions)
            subtype.fromSuper( value ) match
                case Some( value ) => subtype.toSuper( modifier(value) )
                case None => value
    }

    given fieldLensNoaf[T, Sel <: Singleton, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, C](
        using
        frt : FieldRetriever.Aux[Sel, R, Field[T, F, N, S]],
        pdc : ProductDeconstructor.Aux[ T, R, RV ],
        fvr : FieldValueReplacer.Aux[Sel, R, RV, F],
        pcn : ProductConstructor[ C, RV, Nothing, T ],
    ) : Lens[T, ProductShape[T, R, RV, Nothing, Unit, Unit, C], FieldSelector[Sel]] with {
        type Inner = F
        type Out = F

        def retrieve(value: T, component: ProductShape[T, R, RV, Nothing, Unit, Unit, C]): F =
            val field : Field[T, F, N, S] = frt.retrieve(component.fieldDescriptions)
            field.extractor(value)

        def modify(value: T, component: ProductShape[T, R, RV, Nothing, Unit, Unit, C], modifier: F => F): T =
            val inner = retrieve(value, component)
            val newInner = modifier(inner)
            val rv = pdc.deconstruct(value, component.fieldDescriptions)
            val newRV = fvr.replace(component.fieldDescriptions, rv, newInner)
            pcn.construct(component.construct)(newRV)
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

        def retrieve(value: T, component : ProductShape[T, R, RV, AF, AFS, AFE, C]): F =
            val field : Fld = frt.retrieve(component.fieldDescriptions)
            field.extractor(value)

        def modify(value: T, component : ProductShape[T, R, RV, AF, AFS, AFE, C], modifier: F => F): T =
            val inner = retrieve(value, component)
            val newInner = modifier(inner)
            val (af, rv) = pdc.deconstruct(value, (component.afExtractor, component.fieldDescriptions))
            val newRV = fvr.replace(component.fieldDescriptions, rv, newInner)
            pcn.construct(component.construct)(newRV, af)
    }

    given lastSelLense[ T, Component ] : Lens[ T, Component, EmptyTuple ] with {
        type Inner = T
        type Out = T

        def retrieve( value: T, component: Component ): T = value
        def modify( value: T, component: Component, modifier: T => T ): T = modifier(value)
    }

    given productTupleSelector[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, SelHead, SelTail <: Tuple, HI, N <: TypeName, HIS, I, O ](
        using
        crt : ComponentRetriever.Aux[ ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead, Field[ T, HI, N, HIS ] ],
        hLens : Lens.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead, HI, HI ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead *: SelTail ] with {
        type Inner = I
        type Out = O

        def retrieve( value: T, component: ProductShape[ T, R, RV, AF, AFS, AFE, C] ): O =
            val hc : Field[ T, HI, N, HIS ] = crt.retrieve(component)
            val hi = hLens.retrieve(value, component)
            tLens.retrieve(hi, hc.schema.shape)

        def modify( value: T, component: ProductShape[ T, R, RV, AF, AFS, AFE, C], modifier: I => I ): T =
            val hc : Field[ T, HI, N, HIS ] = crt.retrieve(component)
            hLens.modify(value, component, (hi : HI) => {
                tLens.modify(hi, hc.schema.shape, modifier)
            })
    }

    given productTupleSelectorLazy[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, SelHead, SelTail <: Tuple, HI, N <: TypeName, HIS, I, O ](
        using
        crt : ComponentRetriever.Aux[ ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead, LazyField[ T, HI, N ] ],
        hiSch : Schema.Aux[ HI, HIS ],
        hLens : Lens.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead, HI, HI ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C], SelHead *: SelTail ] with {
        type Inner = I
        type Out = O

        def retrieve( value: T, component: ProductShape[ T, R, RV, AF, AFS, AFE, C] ): O =
            val hi = hLens.retrieve(value, component)
            tLens.retrieve(hi, hiSch.shape)

        def modify( value: T, component: ProductShape[ T, R, RV, AF, AFS, AFE, C], modifier: I => I ): T =
            hLens.modify(value, component, (hi : HI) => {
                tLens.modify(hi, hiSch.shape, modifier)
            })
    }

    given coproductTuple[ T, R <: Tuple, RV <: Tuple, D, DN, SelHead, SelTail <: Tuple, HI, DV, N <: TypeName, HIS, I, O ](
        using
        crt : ComponentRetriever.Aux[ CoproductShape[ T, R, RV, D, DN], SelHead, Subtype[ T, HI, D, DN, DV, N, HIS] ],
        hLens : Lens.Aux[ T, CoproductShape[ T, R, RV, D, DN], SelHead, HI, Option[ HI ] ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, CoproductShape[ T, R, RV, D, DN], SelHead *: SelTail ] with {
        type Inner = I
        type Out = Option[ O ]

        def retrieve( value: T, component: CoproductShape[ T, R, RV, D, DN] ): Option[ O ] =
            val hc : Subtype[ T, HI, D, DN, DV, N, HIS] = crt.retrieve(component)
            val hiOpt = hLens.retrieve(value, component)
            hiOpt.map( hi => tLens.retrieve(hi, hc.schema.shape) )

        def modify( value: T, component: CoproductShape[ T, R, RV, D, DN], modifier: I => I ): T =
            val hc : Subtype[ T, HI, D, DN, DV, N, HIS] = crt.retrieve(component)
            hLens.modify(value, component, (hi : HI) => {
                tLens.modify(hi, hc.schema.shape, modifier)
            })
    }

    given coproductTupleLazy[ T, R <: Tuple, RV <: Tuple, D, DN, SelHead, SelTail <: Tuple, HI, DV, N <: TypeName, HIS, I, O ](
        using
        crt : ComponentRetriever.Aux[ CoproductShape[ T, R, RV, D, DN], SelHead, LazySubtype[ T, HI, D, DN, DV, N] ],
        hiSch : Schema.Aux[ HI, HIS ],
        hLens : Lens.Aux[ T, CoproductShape[ T, R, RV, D, DN], SelHead, HI, Option[ HI ] ],
        tLens : Lens.Aux[ HI, HIS, SelTail, I, O ],
    ) : Lens[ T, CoproductShape[ T, R, RV, D, DN], SelHead *: SelTail ] with {
        type Inner = I
        type Out = Option[ O ]

        def retrieve( value: T, component: CoproductShape[ T, R, RV, D, DN] ): Option[ O ] =
            val hiOpt = hLens.retrieve(value, component)
            hiOpt.map( hi => tLens.retrieve(hi, hiSch.shape) )

        def modify( value: T, component: CoproductShape[ T, R, RV, D, DN], modifier: I => I ): T =
            hLens.modify(value, component, (hi : HI) => {
                tLens.modify(hi, hiSch.shape, modifier)
            })
    }

}

trait LensDsl extends SelectorDsl {

    class SelectUtility[T, S, Sel <: Tuple, Inner, Out](
        value : T,
        sch: Schema.Aux[T, S],
        lens: Lens.Aux[T, S, Sel, Inner, Out],
    ) {
        def modify(modifier: Inner => Inner): T = lens.modify(value, sch.shape, modifier)
        def retrieve : Out = lens.retrieve(value, sch.shape)
    }

    extension [T, S](value : T)(using sch : Schema.Aux[T, S])
        def select[Sel <: Tuple, Inner, Out](sel: Selector[Sel])(using lens: Lens.Aux[T, S, Sel, Inner, Out]): SelectUtility[T, S, Sel, Inner, Out] =
            new SelectUtility[T, S, Sel, Inner, Out](value, sch, lens)

}

object LensDsl extends LensDsl

type OptionOf[A] = [B] =>> OptionValue[A, B]

trait OptionValue[A, B] {
    def convert(a: A): B
    def toOption(b: B): Option[A]
}
