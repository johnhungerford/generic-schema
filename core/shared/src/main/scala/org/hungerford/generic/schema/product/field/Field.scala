package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.coproduct.subtype.Subtype
import org.hungerford.generic.schema.{Primitive, Schema, SchemaRebuilder}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

import scala.collection.mutable
import scala.language.higherKinds
import scala.annotation.meta.field
import scala.compiletime.{erasedValue, summonInline}
import org.hungerford.generic.schema.types.SimpleExtractor

type FieldName = Stringleton

sealed case class Field[ T, F, N <: FieldName, S ] private[ schema ] (
    override val fieldName : N,
    override val extractor : T => F,
    override val schema : Schema.Aux[ F, S ],
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ F ] ] = Set.empty[ Validator[ F ] ],
    override val default : Option[ F ] = None,
    override val examples : Seq[ F ] = Nil,
    override val deprecated : Boolean = false,
) extends Field.Shaped[ F, S ] with Field.OrLazy[ T, F, N ] {
    type Self = Field[ T, F, N, S ]
}

sealed case class LazyField[ T, F, N <: FieldName ] private[ schema ] (
    override val fieldName : N,
    override val extractor : T => F,
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ F ] ] = Set.empty[ Validator[ F ] ],
    override val default : Option[ F ] = None,
    override val examples : Seq[ F ] = Nil,
    override val deprecated : Boolean = false,
) extends Field.OrLazy[ T, F, N ] {
    def schema[ S ](
        using
        sch : Schema.Aux[ F, S ]
    ) : Schema.Aux[ F, S ] = sch

    def resolveSchema[ S ]( implicit sch : Schema.Aux[ F, S ] ) : Field[ T, F, N, S ] = {
        Field[ T, F, N, S ](
            fieldName,
            extractor,
            sch,
            description,
            validators,
            default,
            examples,
            deprecated,
        )
    }

    type Self = LazyField[ T, F, N ]
}

object Field {
    sealed trait Field {
        def fieldName : String
        def description : Option[ String ]
        def deprecated : Boolean
    }
    sealed trait For[ T ] extends Field.Field
    sealed trait Of[ F ] extends Field.Field {
        def validators : Set[ Validator[ F ] ]
        def default : Option[ F ]
        def examples : Seq[ F ]
    }
    sealed trait Named[ N <: FieldName ] extends Field.Field {
        def fieldName : N
    }
    sealed trait Shaped[ F, S ] extends Of[ F ] {
        def schema : Schema.Aux[ F, S ]
    }
    sealed trait Extr[ T, F ] extends For[ T ] with Of[ F ] {
        def extractor : T => F
    }
    sealed trait OrLazy[ T, F, N <: FieldName ] extends Extr[ T, F ] with Named[ N ]
}

trait FieldDsl {

    class FieldSchemaRebuilder[ Builder, T, F, N <: FieldName, S ]( builder : Builder, field : Field[ T, F, N, S ] ) {
        def apply[ NewS ]( rebuilder : Builder => Schema.Aux[ F, NewS ] ) : Field[ T, F, N, NewS ] = {
            field.copy[ T, F, N, NewS ]( schema = rebuilder( builder ) )
        }
    }

    extension [ T, F, N <: FieldName, S ]( field : Field[ T, F, N, S ] )
        def apply[ Sel <: Tuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Field[ T, F, N, S ], Sel, Inner ],
        ) : Inner = cr.retrieve( field )


    extension [ T, F, N <: FieldName, S ]( field : Field[ T, F, N, S ] ) {
        def rebuildSchema( using rb: SchemaRebuilder[ F, S ] ): FieldSchemaRebuilder[ rb.Builder, T, F, N, S ] =
            new FieldSchemaRebuilder[ rb.Builder, T, F, N, S ]( rb.rebuild( field.schema ), field )

        def modifySchema[ NewS ](
            modifier: Schema.Aux[ F, S ] => Schema.Aux[ F, NewS ],
        ): Field[ T, F, N, NewS ] = Field[ T, F, N, NewS ](
            field.fieldName,
            field.extractor,
            modifier( field.schema ),
            field.description,
            field.validators,
        )

        def modifyComponent[ Sel <: Tuple, Inner ](
            selector: Selector[ Sel ],
        )(
            using
            cr: ComponentRetriever.Aux[ Field[ T, F, N, S ], Sel, Inner ],
        ): ComponentUpdater.Updater[ Field[ T, F, N, S ], Inner, Sel ] =
            ComponentUpdater.Updater( field )

        def rebuild: FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, T => F ] = FieldBuilder.from( field )

        def withSchema[ S1 ]( sch : Schema.Aux[ F, S1 ] ) : Field[ T, F, N, S1 ] =
            field.copy[ T, F, N, S1 ]( schema = sch )

        def withName[ N1 <: FieldName ]( name : N1 ) : Field[ T, F, N1, S ] =
            field.copy[ T, F, N1, S ]( fieldName = name )
        def withDescription( desc : String ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( description = Some( desc ) )
        def withoutDescription : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( description = None )
        def withValidation( vals : Validator[ F ]* ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( validators = vals.toSet )
        def addValidators( vals : Validator[ F ]* ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( validators = field.validators ++ vals.toSet )
        def withoutValidation : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( validators = Set.empty[ Validator[ F ] ] )
        def withDefault( defaultValue : F ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( default = Some( defaultValue ) )
        def withoutDefault : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( default = None )
        def withExamples( exs : F* ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( examples = exs )
        def addExamples( exs : F* ) : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( examples = field.examples ++ exs )
        def withoutExamples : Field[ T, F, N, S ] =
            field.copy[ T, F, N, S ]( examples = Nil )
    }

    extension [ T, F, N <: FieldName ]( field : LazyField[ T, F, N ] ) {
        def withSchema[ S ]( sch : Schema.Aux[ F, S ] ) : Field[ T, F, N, S ] =
            field.resolveSchema( sch )

        def rebuild: FieldBuilder[ T, F, N, Unit, Deferred, T => F ] = FieldBuilder.from( field )

        def withName[ N1 <: FieldName ]( name : N1 ) : LazyField[ T, F, N1 ] =
            field.copy[ T, F, N1 ]( fieldName = name )
        def withDescription( desc : String ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( description = Some( desc ) )
        def withoutDescription : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( description = None )
        def withValidators( vals : Validator[ F ]* ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( validators = vals.toSet )
        def addValidators( vals : Validator[ F ]* ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( validators = field.validators ++ vals.toSet )
        def withoutValidation : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( validators = Set.empty[ Validator[ F ] ] )
        def withDefault( defaultValue : F ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( default = Some( defaultValue ) )
        def withoutDefault : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( default = None )
        def withExamples( exs : F* ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( examples = exs )
        def addExamples( exs : F* ) : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( examples = field.examples ++ exs )
        def withoutExamples : LazyField[ T, F, N ] =
            field.copy[ T, F, N ]( examples = Nil )
    }

    class FromSchema[ T, F ] {
        def apply[ N <: FieldName ]( fieldName : N, extract : T => F )( using sch : Schema[ F ] ) : Field[ T, F, N, sch.Shape ] = {
            Field[ T, F, N, sch.Shape ]( fieldName, extract, sch )
        }
    }

    class FromPrimitive[ T, F ] {
        def apply[ N <: FieldName ]( fieldName : N, extract : T => F ) : Field[ T, F, N, Unit ] = {
            Field[ T, F, N, Unit ]( fieldName, extract, Primitive[ F ]() )
        }
    }

    extension ( field : Field.type )
        def builder[ T, F ] : FieldBuilder[ T, F, Unit, Unit, Nothing, Unit ] = FieldBuilder[ T, F ]
        def fromSchema[ T, F ] : FromSchema[ T, F ] = new FromSchema[ T, F ]
        def primitive[ T, F ] : FromPrimitive[ T, F ] = new FromPrimitive[ T, F ]

}
