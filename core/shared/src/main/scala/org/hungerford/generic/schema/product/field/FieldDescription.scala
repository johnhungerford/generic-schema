package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist._

import scala.language.higherKinds

trait FieldDescription[ T ] {
    type S <: Schema[ T ]

    val fieldName : String
    val schema : S
    val description : Option[ String ] = None
    val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
}

object FieldDescription {
    type Aux[ T, St <: Schema[ T ] ] = FieldDescription[ T ] { type S = St }
}

case class FieldDescriptionCase[ T, St <: Schema[ T ] ](
    override val fieldName : String,
    override val schema : St,
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends FieldDescription[ T ] {
    type S = St
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

object FieldNameExtractor extends Poly1 {
    implicit def fieldNameCase[ T, Rt ] : Case.Aux[ FieldDescription[ T ], String ] = at[ FieldDescription[ T ] ]( _.fieldName )
}

object DescriptionExtractor extends Poly1 {
    implicit def descriptionCase[ T, Rt ] : Case.Aux[ FieldDescription[ T ], Option[ String ] ] = at[ FieldDescription[ T ] ]( _.description )
}

class FieldDescriptionMapper[ OtherSchema[ _ ] ] extends Poly1 {
    implicit val hnilCase : Case.Aux[ HNil, HNil ] = at[ HNil ]( identity )

    implicit def genericCase[ T, S <: Schema[ T ] ](
        implicit ft : FieldTranslator[ T, OtherSchema ],
    ) : Case.Aux[ FieldDescription.Aux[ T, S ], TranslatedFieldDescription[ T, OtherSchema ] ] =
        at[ FieldDescription.Aux[ T, S ] ]( ( fd : FieldDescription[ T ] ) => ft.translate( fd ) )
}

object FieldDescriptionMapper {
    def apply[ OtherSchema[ _ ] ] : FieldDescriptionMapper[ OtherSchema ] = new FieldDescriptionMapper[ OtherSchema ]
}
