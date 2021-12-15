package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldRetriever}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

case class CoproductShape[ T, R <: Tuple, RV <: Tuple, D, DN, DR <: Tuple ](
    subtypeDescriptions : R,
    discriminatorValues : DR,
)(
    using
    ctx : CtxWrapTuplesConstraint[ Subtype, R, RV ],
    dEv : ValidDiscriminator[ D, DN, R ],
    drEv : ValidDiscriminatorValues[ D, DR ],
)
