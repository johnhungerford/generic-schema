package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, FieldName, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.validator.Validator

trait ProductSchemaDsl {

    extension [ T, R <: Tuple, RV <: Tuple, AF, AFS, C ]( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, T => Map[ String, AF ], C ] ] )
        def modifyAdditionalFieldsSchema[ NewAFS ](
            modifier : Schema.Aux[ AF, AFS ] => Schema.Aux[ AF, NewAFS ],
        )(
            using
            ctx : CtxWrapTuplesConstraint[ Field.Of, R, RV ],
            consChoice : ConstrUpdateChoice.Aux[ RV, RV, AF, AF, C, C ],
            uniq : => UniqueFieldNames[ R ],
            pc : ProductConstructor[ C, RV, AF, T ],
        ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, NewAFS, T => Map[ String, AF ], C ] ] = {
            ProductSchemaBuilder.from( schema )
              .additionalFields[ AF ]
              .fromSchema[ NewAFS ]( schema.shape.afExtractor )( modifier( schema.shape.additionalFieldsSchema ) )
              .build
        }

    extension [ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ]( builder : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] )
        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], Inner, Sel ] =
            ComponentUpdater.Updater( builder )

}
