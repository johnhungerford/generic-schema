package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.Field.Aux
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds
import scala.annotation.meta.field

import scala.compiletime.{erasedValue, summonInline}
import org.hungerford.generic.schema.types.SimpleExtractor

case class TranslatedFieldDescription[ T, OtherSchema[ _ ] ](
    fieldName : String,
    schema : OtherSchema[ T ],
    description : Option[ String ] = None,
    validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
)

trait FieldTranslator[ T, N <: FieldName, S, OtherSchema[ _ ] ] {
    def translate( fd : Field.Aux[ T, N, S ] ) : TranslatedFieldDescription[ T, OtherSchema ]
}

object FieldTranslator {
    def apply[ T, N <: FieldName, S, OtherSchema[ _ ] ](
        using
        ft : FieldTranslator[ T, N, S, OtherSchema ],
    ) : FieldTranslator[ T, N, S, OtherSchema ] = ft

    def genericFieldTranslator[ T, N <: FieldName, S, OtherSchema[ _ ] ](
        using
        osc : OtherSchema[ T ],
    ) : FieldTranslator[ T, N, S, OtherSchema ] = new FieldTranslator[ T, N, S, OtherSchema ] {
        def translate( fd : Field.Aux[ T, N, S ] ) : TranslatedFieldDescription[ T, OtherSchema ] = {
            TranslatedFieldDescription(
                fd.fieldName,
                osc,
                fd.description,
                fd.validators,
            )
        }
    }

    given genericSchemaFieldTranslator[ T, N <: FieldName, S, OtherSchema[ _ ] ](
        using
        schTrans : SchemaTranslator[ T, S, OtherSchema ],
    ) : FieldTranslator[ T, N, S, OtherSchema ] with
        def translate( fd : Field.Aux[ T, N, S ] ) : TranslatedFieldDescription[ T, OtherSchema ] = {
            genericFieldTranslator[ T, N, S, OtherSchema ]( using schTrans.translate( fd.schema ) ).translate( fd )
        }

}

trait FieldTupleTranslator[ FS <: Tuple, OtherSchema[ _ ] ] {
    type TFS <: Tuple

    def translate( fields : FS ) : TFS
}

object FieldTupleTranslator {
    type Aux[ FSt <: Tuple, OtherSchema[ _ ], TFSt <: Tuple ] = FieldTupleTranslator[ FSt, OtherSchema ] { type TFS = TFSt }

    given [ OtherSchema[ _ ] ] : FieldTupleTranslator[ EmptyTuple, OtherSchema ] with
        type TFS = EmptyTuple
        def translate( fields : EmptyTuple ) : EmptyTuple = EmptyTuple
    
    given [ T, N <: FieldName, S, FTail <: Tuple, TFTail <: Tuple, OtherSchema[ _ ] ](
        using
        ft : => FieldTranslator[ T, N, S, OtherSchema ],
        nt : FieldTupleTranslator.Aux[ FTail, OtherSchema, TFTail ],
    ) : FieldTupleTranslator[ Field.Aux[ T, N, S ] *: FTail, OtherSchema ] with
        type TFS = TranslatedFieldDescription[ T, OtherSchema ] *: TFTail

        def translate( fields : Field.Aux[ T, N, S ] *: FTail ) : TranslatedFieldDescription[ T, OtherSchema ] *: TFTail = {
            val translatedHead = ft.translate( fields.head )
            val translatedTail = nt.translate( fields.tail )
            translatedHead *: translatedTail
        }

    transparent inline def translateFieldDescriptions[ R <: Tuple, OtherSchema[ _ ] ](
        fieldDescriptions : R,
    )(
        using
        tr : FieldTupleTranslator[ R, OtherSchema ]
    ) : tr.TFS = {
        tr.translate( fieldDescriptions )
    }
}
