package org.hungerford.generic.schema.product.field

import scala.compiletime.{summonInline, erasedValue}
import org.hungerford.generic.schema.types.SimpleExtractor

trait FieldExtractor[ Source, T, OtherSchema[ _ ] ] {
    type Out

    def extract( from : Source, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : Out
}

trait FieldsExtractor[ Source, R <: Tuple ] {
    type Out <: Tuple

    def extract( from : Source, informedBy : R ) : Out
}

object FieldsExtractor {
  
    given [ Source ] : FieldsExtractor[ Source, EmptyTuple ] with {
        type Out = EmptyTuple

        def extract( from : Source, informedBy : EmptyTuple ) : EmptyTuple = EmptyTuple
    }

    given [ Source, T, OtherSchema[ _ ], NextR <: Tuple ](
        using
        fe : FieldExtractor[ Source, T, OtherSchema ],
        next : FieldsExtractor[ Source, NextR ],
    ) : FieldsExtractor[ Source, TranslatedFieldDescription[ T, OtherSchema ] *: NextR ] with {
        type Out = fe.Out *: next.Out

        def extract( from : Source, informedBy : TranslatedFieldDescription[ T, OtherSchema ] *: NextR ) : Out = {
            val headValue = fe.extract( from, informedBy.head )
            val tailValue = next.extract( from, informedBy.tail )
            headValue *: tailValue
        }
    }

    def extract[ Source, R <: Tuple ]( from : Source, informedBy : R )(
        using
        fse : FieldsExtractor[ Source, R ],
    ) : fse.Out = fse.extract( from, informedBy )

}
