package org.hungerford.generic.schema

import org.hungerford.generic.schema.validator.Validator

trait Schema[ T ] {
    /**
     * Schema types of components (for products or coproducts)
     */
    type R

    // fields
    def genericDescription : Option[ String ]
    def genericValidators : Set[ Validator[ T ] ]

    // updaters
    def withDescription( description : String ) : Schema[ T ]
    def withoutDescription : Schema[ T ]
    def withValidation( validators : Validator[ T ]* ) : Schema[ T ]
    def withoutValidation : Schema[ T ]

    // Fully typed version
    def aux : Schema.Aux[ T, R ] = this : Schema.Aux[ T, R ]
}

case object NoSchema extends Schema[ Nothing ] {
    override type R = Nothing

    val genericDescription : Option[ String ] = None
    val genericValidators : Set[ Validator[ Nothing ] ] = Set.empty[ Validator[ Nothing ] ]

    override def withDescription( description : String ) : Schema[ Nothing ] = this

    override def withValidation( validators : Validator[ Nothing ]* ) : Schema[ Nothing ] = this

    override def withoutDescription : Schema[ Nothing ] = this

    override def withoutValidation : Schema[ Nothing ] = this
}

case class Primitive[ T ](
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends Schema[ T ] {
    override type R = Nothing

    override def withDescription( description : String ) : Schema[ T ] =
        copy( genericDescription = Some( description ) )

    override def withValidation( validators : Validator[ T ]* ) : Schema[ T ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override def withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override def withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )
}

object Schema {
    def empty[ T ] : Primitive[ T ] = Primitive[ T ]()

    def apply[ T ]( implicit schema : Schema[ T ] ) : Schema[ T ] = schema
    def aux[ T, Rt ]( implicit schema : Schema.Aux[ T, Rt ] ) : Schema.Aux[ T, Rt ] = schema

    type Aux[ T, Rt ] = Schema[ T ] { type R = Rt }
}


