package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}

trait CoproductSchemaDsl {

    extension [ T, R <: Tuple, D, DN ]( builder : CoproductSchemaBuilder[ T, R, D, DN ] )
        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ CoproductSchemaBuilder[ T, R, D, DN ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ CoproductSchemaBuilder[ T, R, D, DN ], Inner, Sel ] =
            ComponentUpdater.Updater( builder )

}
