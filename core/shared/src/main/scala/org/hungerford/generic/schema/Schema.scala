package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.{CtxWrapTuplesConstraint, ProductSchemaBuilder, ProductShape}
import org.hungerford.generic.schema.product.field.FieldReplacer
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

sealed trait Schema[ T ] {

    // Internal representation of components
    type Shape

    def shape : Shape

    // fields
    def genericDescription : Option[ String ]
    def genericValidators : Set[ Validator[ T ] ]

    // updaters
    private[ schema ] def _withDescription( description : String ) : Schema[ T ]
    private[ schema ] def _withoutDescription : Schema[ T ]
    private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema[ T ]
    private[ schema ] def _withoutValidation : Schema[ T ]

    given givenSchema : Schema.Aux[ T, Shape ] = this

}

case object NoSchema extends Schema[ Nothing ] {

    override type Shape = Unit
    override def shape : Unit = ()

    val genericDescription : Option[ String ] = None
    val genericValidators : Set[ Validator[ Nothing ] ] = Set.empty[ Validator[ Nothing ] ]

    override private[ schema ] def _withDescription( description : String ) : Schema[ Nothing ] = this

    override private[ schema ] def _withValidation( validators : Validator[ Nothing ]* ) : Schema[ Nothing ] = this

    override private[ schema ] def _withoutDescription : Schema[ Nothing ] = this

    override private[ schema ] def _withoutValidation : Schema[ Nothing ] = this
}

final case class Primitive[ T ](
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends Schema[ T ] {
    override type Shape = Unit
    override def shape : Unit = ()

    override private[ schema ] def _withDescription( description : String ) : Schema[ T ] =
        copy( genericDescription = Some( description ) )

    override private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema[ T ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override private[ schema ] def _withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override private[ schema ] def _withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )

}

final case class ComplexSchema[ T, S ](
    shape : S,
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends Schema[ T ] {
    override type Shape = S

    override private[ schema ] def _withDescription( description : String ) : Schema[ T ] = copy( genericDescription = Some( description ) )

    override private[ schema ] def _withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema[ T ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override private[ schema ] def _withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )
}

object Schema {
    type Aux[ T, S ] = Schema[ T ] { type Shape = S }

    def empty[ T ] : Primitive[ T ] = Primitive[ T ]()

    def apply[ T ]( implicit schema : Schema[ T ] ) : Schema[ T ] = schema
}

trait SchemaDsl {

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def apply[ Sel <: Tuple ](
            selector : Selector[ Sel ],
        )(
            using
            crt : ComponentRetriever[ Schema.Aux[ T, S ], Sel ],
        ) : crt.Inner = crt.retrieve( schema )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withDescription( description : String ) : Schema[ T ] = schema._withDescription( description )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutDescription : Schema[ T ] = schema._withoutDescription

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withValidation( validators : Validator[ T ]* ) : Schema[ T ] = schema._withValidation( validators : _* )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutValidation : Schema[ T ] = schema._withoutValidation

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def rebuild( using srb : SchemaRebuilder[ T, S ] ) : srb.Builder = srb.rebuild( schema )

    case class ComponentModifier[ T, S, Sel <: Tuple, Inner ](
        schema : Schema.Aux[ T, S ],
    ) {
        def apply[ NewInner, NewS ](
            updater : Inner => NewInner,
        )(
            using
            cu : => ComponentUpdater.Aux[ Schema.Aux[ T, S ], Sel, Inner, NewInner, Schema.Aux[ T, NewS ] ],
        ) : Schema.Aux[ T, NewS ] = {
            cu.update( schema )( updater )
        }
    }

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            crt : ComponentRetriever.Aux[ Schema.Aux[ T, S ], Sel, Inner ],
        ) : ComponentModifier[ T, S, Sel, Inner ] = ComponentModifier[ T, S, Sel, Inner ](
            schema,
        )

    extension ( sch : Schema.type )
        def productBuilder[ T ] : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ] =
            ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ](
                aftSch = NoSchema,
                fieldDescs = EmptyTuple,
                constr = (),
                decons = (),
            )

    extension ( sch : Schema.type )
        def primitiveBuilder[ T ] : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]()

    extension ( sch : Schema.type )
        def derivedBuilder[ T ]( using bldDeriv : SchemaBuildDeriver[ T ] ) : bldDeriv.Builder = bldDeriv.derive

    extension ( sch : Schema.type )
        def derived[ T ]( using sd : SchemaDeriver[ T ] ) : Schema.Aux[ T, sd.Shape ] = sd.derive

}
