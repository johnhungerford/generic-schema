package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.empty.*
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.validator.Validator

import scala.collection.mutable
import scala.language.higherKinds

trait FieldDescription[ T ] {
    type Name <: String
    type Shape

    val fieldName : Name
    val schema : Schema.Aux[ T, Shape ]
    val description : Option[ String ] = None
    val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
}

object FieldDescription {
    type Aux[ T, N <: String, S ] = FieldDescription[ T ] { type Name = N; type Shape = S }
    type AuxN[ T, N <: String ] = FieldDescription[ T ] { type Name = N }
    type AuxS[ T, S ] = FieldDescription[ T ] { type Shape = S }
}

case class FieldDescriptionCase[ T, N <: String, S ](
    override val fieldName : N,
    override val schema : Schema.Aux[ T, S ],
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends FieldDescription[ T ] {
    type Name = N
    type Shape = S
}

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

}
