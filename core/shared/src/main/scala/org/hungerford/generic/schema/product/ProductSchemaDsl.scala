package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.{BuildableFieldBuilder, Field, FieldBuilder, FieldName, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

trait ProductSchemaDsl {

    extension [ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ]( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] )
        def modifyAdditionalFieldsSchema[ NewAFS ](
            modifier : Schema.Aux[ AF, AFS ] => Schema.Aux[ AF, NewAFS ],
        )(
            using
            ctx : CtxWrapTuplesConstraint[ Field, R, RV ],
            consChoice : ConstrUpdateChoice.Aux[ RV, RV, AF, AF, C, DC, C, DC ],
            lengther : TupleIntLength[ R ],
            uniq : => UniqueFieldNames[ R ],
            pc : ProductConstructor[ C, RV, AF, T ],
            pdc : ProductDeconstructor[ DC, RV, AF, T ],
        ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, NewAFS, C, DC ] ] = {
            ProductSchemaBuilder.from( schema )
              .additionalFields[ AF ]
              .fromSchema[ NewAFS ]( modifier( schema.shape.additionalFieldsSchema ) )
              .build
        }

}
