package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldDescription.Aux
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

trait FieldTranslator[ T, S, OtherSchema[ _ ] ] {
    def translate( fd : FieldDescription.AuxS[ T, S ] ) : TranslatedFieldDescription[ T, OtherSchema ]
}

object FieldTranslator {
    def apply[ T, S, OtherSchema[ _ ] ](
        using
        ft : FieldTranslator[ T, S, OtherSchema ],
    ) : FieldTranslator[ T, S, OtherSchema ] = ft

    def genericFieldTranslator[ T, S, OtherSchema[ _ ] ](
        using
        osc : OtherSchema[ T ],
    ) : FieldTranslator[ T, S, OtherSchema ] = new FieldTranslator[ T, S, OtherSchema ] {
        def translate( fd : FieldDescription.AuxS[ T, S ] ) : TranslatedFieldDescription[ T, OtherSchema ] = {
            TranslatedFieldDescription(
                fd.fieldName,
                osc,
                fd.description,
                fd.validators,
            )
        }
    }

    given genericSchemaFieldTranslator[ T, S, OtherSchema[ _ ] ](
        using
        schTrans : SchemaTranslator[ T, S, OtherSchema ],
    ) : FieldTranslator[ T, S, OtherSchema ] with
        def translate( fd : FieldDescription.AuxS[ T, S ] ) : TranslatedFieldDescription[ T, OtherSchema ] = {
            genericFieldTranslator[ T, S, OtherSchema ]( using schTrans.translate( fd.schema ) ).translate( fd )
        }

    transparent inline def translateFieldDescriptions[ R <: Tuple, OtherSchema[ _ ] ](
        fieldDescriptions : R
    ) : Tuple = {
        inline fieldDescriptions match {
            case EmptyTuple => EmptyTuple
            case fds : (FieldDescription.AuxS[ t, s ] *: fts) =>
                val fieldTranslator = summonInline[ FieldTranslator[ t, s, OtherSchema ] ]
                fieldTranslator.translate( fds.head ) *: translateFieldDescriptions[ fts, OtherSchema ]( fds.tail )
        }
    }

}
