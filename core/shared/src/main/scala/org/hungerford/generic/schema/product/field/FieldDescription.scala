package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist._

import scala.language.higherKinds

trait FieldDescription[ T ] {
    type R

    val fieldName : String
    val schema : Schema.Aux[ T, R ]
    val description : Option[ String ] = None
    val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
}

object FieldDescription {
    type Aux[ T, Rt ] = FieldDescription[ T ] { type R = Rt }
}

case class FieldDescriptionCase[ T, Rt ](
    override val fieldName : String,
    override val schema : Schema.Aux[ T, Rt ],
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends FieldDescription[ T ] {
    type R = Rt
}

case class TranslatedFieldDescription[ T, OtherSchema[ _ ] ](
    fieldName : String,
    schema : OtherSchema[ T ],
    description : Option[ String ] = None,
    validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
)

trait FieldTranslator[ T, OtherSchema[ _ ] ] {
    def translate( fd : FieldDescription[ T ] ) : TranslatedFieldDescription[ T, OtherSchema ]
}

object FieldTranslator {
    def apply[ T, OtherSchema[ _ ] ](
        implicit ft : FieldTranslator[ T, OtherSchema ],
    ) : FieldTranslator[ T, OtherSchema ] = ft

    implicit def genericFieldTranslator[ T, OtherSchema[ _ ] ](
        implicit osc : OtherSchema[ T ],
    ) : FieldTranslator[ T, OtherSchema ] = {

        ( fd : FieldDescription[ T ] ) => {
            TranslatedFieldDescription(
                fd.fieldName,
                osc,
                fd.description,
                fd.validators,
            )
        }

    }
}

class FieldDescriptionMapper[ OtherSchema[ _ ] ] extends Poly1 {
    implicit val hnilCase : Case.Aux[ HNil, HNil ] = at[ HNil ]( identity )

    implicit def genericCase[ T, Rt ](
        implicit ft : FieldTranslator[ T, OtherSchema ],
    ) : Case.Aux[ FieldDescription[ T ], TranslatedFieldDescription[ T, OtherSchema ] ] =
        at[ FieldDescription[ T ] ]( ( fd : FieldDescription[ T ] ) => ft.translate( fd ) )
}

object FieldDescriptionMapper {
    def apply[ OtherSchema[ _ ] ] : FieldDescriptionMapper[ OtherSchema ] = new FieldDescriptionMapper[ OtherSchema ]
}
