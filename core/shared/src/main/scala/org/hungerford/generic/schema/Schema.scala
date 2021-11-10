package org.hungerford.generic.schema

import org.hungerford.generic.schema.empty._
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds

sealed trait Schema[ T ] {

    // Internal representation of components
    type Shape

    def shape : Shape

    // fields
    def genericDescription : Option[ String ]
    def genericValidators : Set[ Validator[ T ] ]

    // updaters
    def withDescription( description : String ) : Schema[ T ]
    def withoutDescription : Schema[ T ]
    def withValidation( validators : Validator[ T ]* ) : Schema[ T ]
    def withoutValidation : Schema[ T ]

}

case object NoSchema extends Schema[ Nothing ] {

    override type Shape = Unit
    override def shape : Unit = ()

    val genericDescription : Option[ String ] = None
    val genericValidators : Set[ Validator[ Nothing ] ] = Set.empty[ Validator[ Nothing ] ]

    override def withDescription( description : String ) : Schema[ Nothing ] = this

    override def withValidation( validators : Validator[ Nothing ]* ) : Schema[ Nothing ] = this

    override def withoutDescription : Schema[ Nothing ] = this

    override def withoutValidation : Schema[ Nothing ] = this
}

final case class Primitive[ T ](
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends Schema[ T ] {
    override type Shape = Unit
    override def shape : Unit = ()

    override def withDescription( description : String ) : Schema[ T ] =
        copy( genericDescription = Some( description ) )

    override def withValidation( validators : Validator[ T ]* ) : Schema[ T ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override def withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override def withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )

}

final case class ComplexSchema[ T, S ](
    shape : S,
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends Schema[ T ] {
    override type Shape = S

    override def withDescription( description : String ) : Schema[ T ] = copy( genericDescription = Some( description ) )

    override def withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override def withValidation( validators : Validator[ T ]* ) : Schema[ T ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override def withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )
}

object Schema {
    type Aux[ T, S ] = Schema[ T ] { type Shape = S }

    def empty[ T ] : Primitive[ T ] = Primitive[ T ]()

    def apply[ T ]( implicit schema : Schema[ T ] ) : Schema[ T ] = schema
}


