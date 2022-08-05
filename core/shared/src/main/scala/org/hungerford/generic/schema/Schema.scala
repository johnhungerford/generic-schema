package org.hungerford.generic.schema

import org.hungerford.generic.schema.coproduct.CoproductSchemaBuilder
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}
import org.hungerford.generic.schema.product.field.FieldReplacer
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.singleton.{SingletonSchemaBuilder, SingletonShape}
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.types.Validation
import org.hungerford.generic.schema.validator.Validator

sealed trait Schema[ T ] {

    // Internal representation of components
    type Shape

    def shape : Shape

    // fields
    def name : Option[ String ]
    def genericDescription : Option[ String ]
    def genericValidators : Set[ Validator[ T ] ]
    def genericExamples : Seq[ T ]
    def deprecated : Boolean

    // updaters
    private[ schema ] def _withName( name : String ) : Schema.Aux[ T, Shape ]
    private[ schema ] def _withoutName : Schema.Aux[ T, Shape ]
    private[ schema ] def _withDescription( description : String ) : Schema.Aux[ T, Shape ]
    private[ schema ] def _withoutDescription : Schema.Aux[ T, Shape ]
    private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema.Aux[ T, Shape ]
    private[ schema ] def _withoutValidation : Schema.Aux[ T, Shape ]
    private[ schema ] def _withExamples( examples : T* ) : Schema.Aux[ T, Shape ]
    private[ schema ] def _withoutExamples : Schema.Aux[ T, Shape ]
    private[ schema ] def _withDeprecated : Schema.Aux[ T, Shape ]
    private[ schema ] def _withoutDeprecated : Schema.Aux[ T, Shape ]

    given givenSchema : Schema.Aux[ T, Shape ] = this

}

case object NoSchema extends Schema[ Nothing ] {

    override type Shape = Unit
    override def shape : Unit = ()

    val name : Option[ String ] = None
    val genericDescription : Option[ String ] = None
    val genericValidators : Set[ Validator[ Nothing ] ] = Set.empty[ Validator[ Nothing ] ]
    val genericExamples : Seq[ Nothing ] = Nil
    val deprecated : Boolean = false

    private[ schema ] def _withName( name : String ) : Schema.Aux[ Nothing, Unit ] = this

    private[ schema ] def _withoutName : Schema.Aux[ Nothing, Unit ] = this

    override private[ schema ] def _withDescription( description : String ) : Schema.Aux[ Nothing, Unit ] = this

    override private[ schema ] def _withValidation( validators : Validator[ Nothing ]* ) : Schema.Aux[ Nothing, Unit ] = this

    override private[ schema ] def _withoutDescription : Schema.Aux[ Nothing, Unit ] = this

    override private[ schema ] def _withoutValidation : Schema.Aux[ Nothing, Unit ] = this

    private[ schema ] def _withExamples( examples : Nothing* ) : Schema.Aux[ Nothing, Unit ] = this

    private[ schema ] def _withoutExamples : Schema.Aux[ Nothing, Unit ] = this

    private[ schema ] def _withDeprecated : Schema.Aux[ Nothing, Unit ] = this

    private[ schema ] def _withoutDeprecated : Schema.Aux[ Nothing, Unit ] = this
}

final case class Primitive[ T ](
    name : Option[ String ] = None,
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    genericExamples : Seq[ T ] = Nil,
    deprecated : Boolean = false,
) extends Schema[ T ] {
    override type Shape = Unit
    override def shape : Unit = ()

    private[ schema ] def _withName( name : String ) : Schema.Aux[ T, Shape ] = copy( name = Some( name ) )

    private[ schema ] def _withoutName : Schema.Aux[ T, Shape ] = copy( name = None )

    override private[ schema ] def _withDescription( description : String ) : Schema.Aux[ T, Shape ] =
        copy( genericDescription = Some( description ) )

    override private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema.Aux[ T, Shape ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override private[ schema ] def _withoutDescription : Schema.Aux[ T, Shape ] = copy( genericDescription = None )

    override private[ schema ] def _withoutValidation : Schema.Aux[ T, Shape ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )

    override private[ schema ] def _withExamples( examples : T* ) : Schema.Aux[ T, Shape ] =
        copy( genericExamples = genericExamples ++ examples )

    override private[ schema ] def _withoutExamples : Schema.Aux[ T, Shape ] = copy( genericExamples = Nil )

    private[ schema ] def _withDeprecated : Schema.Aux[ T, Shape ] = copy( deprecated = true )

    private[ schema ] def _withoutDeprecated : Schema.Aux[ T, Shape ] = copy( deprecated = false )

}

final case class ComplexSchema[ T, S ](
    shape : S,
    name : Option[ String ] = None,
    genericDescription : Option[ String ] = None,
    genericValidators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    genericExamples : Seq[ T ] = Nil,
    deprecated : Boolean = false,
) extends Schema[ T ] {
    override type Shape = S

    private[ schema ] def _withName( name : String ) : Schema.Aux[ T, Shape ] = copy( name = Some( name ) )

    private[ schema ] def _withoutName : Schema.Aux[ T, Shape ] = copy( name = None )

    override private[ schema ] def _withDescription( description : String ) : Schema.Aux[ T, Shape ] = copy( genericDescription = Some( description ) )

    override private[ schema ] def _withoutDescription : Schema.Aux[ T, Shape ] = copy( genericDescription = None )

    override private[ schema ] def _withValidation( validators : Validator[ T ]* ) : Schema.Aux[ T, Shape ] =
        copy( genericValidators = genericValidators ++ validators.toSet )

    override private[ schema ] def _withoutValidation : Schema.Aux[ T, Shape ] = copy( genericValidators = Set.empty[ Validator[ T ] ] )

    override private[ schema ] def _withExamples( examples : T* ) : Schema.Aux[ T, Shape ] =
        copy( genericExamples = genericExamples ++ examples )

    override private[ schema ] def _withoutExamples : Schema.Aux[ T, Shape ] = copy( genericExamples = Nil )

    private[ schema ] def _withDeprecated : Schema.Aux[ T, Shape ] = copy( deprecated = true )

    private[ schema ] def _withoutDeprecated : Schema.Aux[ T, Shape ] = copy( deprecated = false )
}

object Schema {
    type Aux[ T, S ] = Schema[ T ] { type Shape = S }

    def empty[ T ] : Primitive[ T ] = Primitive[ T ]()

    def apply[ T ]( implicit schema : Schema[ T ] ) : Schema.Aux[ T, schema.Shape ] = schema
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
        def withName( name : String ) : Schema.Aux[ T, S ] = schema._withName( name )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutName : Schema.Aux[ T, S ] = schema._withoutName

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withDescription( description : String ) : Schema.Aux[ T, S ] = schema._withDescription( description )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutDescription : Schema.Aux[ T, S ] = schema._withoutDescription

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withValidation( validators : Validator[ T ]* ) : Schema.Aux[ T, S ] = schema._withValidation( validators : _* )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutValidation : Schema.Aux[ T, S ] = schema._withoutValidation

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withExamples( examples : T* ) : Schema.Aux[ T, S ] = schema._withExamples( examples : _* )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutExamples : Schema.Aux[ T, S ] = schema._withoutExamples

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withDeprecated : Schema.Aux[ T, S ] = schema._withDeprecated

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def withoutDeprecated : Schema.Aux[ T, S ] = schema._withoutDeprecated

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def rebuild( using srb : SchemaRebuilder[ T, S ] ) : srb.Builder = srb.rebuild( schema )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def modifyComponent[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            crt : ComponentRetriever.Aux[ Schema.Aux[ T, S ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ Schema.Aux[ T, S ], Inner, Sel ] = ComponentUpdater.Updater(
            schema,
        )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def extractSchema[ InnerT ](
            using extr: SchemaExtractor[ InnerT, Schema.Aux[ T, S ] ]
        ) : Schema.Aux[ InnerT, extr.Shape ] = extr.extract( schema )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def as[ OtherSchema[ _ ] ](
            using tr : SchemaTranslator[ T, S, OtherSchema ],
        ) : OtherSchema[ T ] = tr.translate( schema )

    extension [ T, S ]( schema : Schema.Aux[ T, S ] )
        def validate(
            value: T,
        )(
            using valid : Validation[ T, Schema.Aux[T, S] ],
        ) : Boolean = valid.isValid( value, schema )

    extension ( sch : Schema.type )
        def productBuilder[ T ] : ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ] =
            ProductSchemaBuilder[ T, EmptyTuple, EmptyTuple, Nothing, Unit, Unit, Unit ](
                aftSch = NoSchema,
                afe = (),
                fieldDescs = EmptyTuple,
                constr = (),
            )

    extension ( sch : Schema.type )
        def coproductBuilder[ T ] : CoproductSchemaBuilder[ T, EmptyTuple, Unit, Unit ] =
            CoproductSchemaBuilder[ T, EmptyTuple, Unit, Unit ]( sts = EmptyTuple )

    extension ( sch : Schema.type )
        def singletonBuilder[ T <: Singleton ] : SingletonSchemaBuilder[ T, Unit ] =
            SingletonSchemaBuilder.empty
        def singletonBuilderOf[ T <: Singleton ]( value : T ) : SingletonSchemaBuilder[ T, Unit ] =
            SingletonSchemaBuilder.empty
        def singleton[ T <: Singleton, N <: TypeName ]( value : T, name : N ) : Schema.Aux[ T , SingletonShape[ T, N ] ] =
            ComplexSchema(
                shape = SingletonShape[ T, N ]( name, value ),
            )

    extension ( sch : Schema.type )
        def primitiveBuilder[ T ] : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]()
        def primitive[ T ] : Schema.Aux[ T, Unit ] = Primitive[ T ]()

    extension ( sch : Schema.type )
        def derivedBuilder[ T ]( using bldDeriv : SchemaBuildDeriver[ T ] ) : bldDeriv.Builder = bldDeriv.derive

    extension ( sch : Schema.type )
        def derived[ T ]( using sd : SchemaDeriver[ T ] ) : Schema.Aux[ T, sd.Shape ] = sd.derive

}
