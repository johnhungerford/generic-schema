package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.coproduct.subtype.Subtype
import org.hungerford.generic.schema.{Primitive, Schema, SchemaRebuilder}
import org.hungerford.generic.schema.translation.SchemaFranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.Field.Aux
import org.hungerford.generic.schema.selector.{ComponentRetriever, ComponentUpdater, Selector}
import org.hungerford.generic.schema.validator.Validator

import scala.collection.mutable
import scala.language.higherKinds
import scala.annotation.meta.field
import scala.compiletime.{erasedValue, summonInline}
import org.hungerford.generic.schema.types.SimpleExtractor

type FieldName = Stringleton

sealed trait Field[ T, F ] {
    type Name <: FieldName
    type Shape
}

sealed case class FieldCase[ T, F, N <: FieldName, S ] private[ schema ] (
    fieldName : N,
    extractor : T => F,
    schema : Schema.Aux[ F, S ],
    description : Option[ String ] = None,
    validators : Set[ Validator[ F ] ] = Set.empty[ Validator[ F ] ],
    default : Option[ F ] = None,
    examples : Seq[ F ] = Nil,
    deprecated : Boolean = false,
) extends Field[ T, F ] {
    type Name = N
    type Shape = S
}

object Field {
    type Aux[ T, F, N <: FieldName, S ] = Field[ T, F ] { type Name = N; type Shape = S }
    type Ctx[ T ] = [ X ] =>> Field[ T, X ]

    def apply[ T, F, N <: FieldName, S ](
        name : N,
        extractor : T => F,
        schema : Schema.Aux[ F, S ],
        description : Option[ String ] = None,
        validators : Set[ Validator[ F ] ] = Set.empty[ Validator[ F ] ],
    ) : Field.Aux[ T, F, N, S ] = FieldCase[ T, F, N, S ]( name, extractor, schema, description, validators )
}

trait FieldDsl {
    
    class FieldSchemaRebuilder[ Builder, T, F, N <: FieldName, S ]( builder : Builder, field : FieldCase[ T, F, N, S ] ) {
        def apply[ NewS ]( rebuilder : Builder => Schema.Aux[ F, NewS ] ) : Field.Aux[ T, F, N, NewS ] = {
            field.copy[ T, F, N, NewS ]( schema = rebuilder( builder ) )
        }
    }

    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def apply[ Sel <: Fuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Field.Aux[ T, F, N, S ], Sel, Inner ],
        ) : Inner = cr.retrieve( field )
        
    
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def rebuildSchema( using rb : SchemaRebuilder[ F, S ] ) : FieldSchemaRebuilder[ rb.Builder, T, F, N, S ] =
            field match {
                case f : FieldCase[ T, F, N, S ] @unchecked =>
                    new FieldSchemaRebuilder[ rb.Builder, T, F, N, S ]( rb.rebuild( f.schema ), f )
            }

    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def modifySchema[ NewS ](
            modifier : Schema.Aux[ F, S ] => Schema.Aux[ F, NewS ],
        ) : Field.Aux[ T, F, N, NewS ] = FieldCase[ T, F, N, NewS ](
            field.fieldName,
            modifier( field.schema ),
            field.description,
            field.validators,
        )

    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def modifyComponent[ Sel <: Fuple, Inner ](
            selector : Selector[ Sel ],
        )(
            using
            cr : ComponentRetriever.Aux[ Field.Aux[ T, F, N, S ], Sel, Inner ],
        ) : ComponentUpdater.Updater[ Field.Aux[ T, F, N, S ], Inner, Sel ] =
            ComponentUpdater.Updater( field )

    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def rebuild : BuildableFieldBuilder[ T, F, N, S ] = FieldBuilder.from( field )

    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withName[ N1 <: FieldName ]( name : N1 ) : Field.Aux[ T, F, N1, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N1, S ]( fieldName = name ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withSchema[ S1 ]( sch : Schema.Aux[ F, S1 ] ) : Field.Aux[ T, F, N, S1 ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S1 ]( schema = sch ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withDescription( desc : String ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( description = Some( desc ) ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withoutDescription : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( description = None ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withValidators( vals : Validator[ F ]* ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( validators = vals.toSet ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def addValidators( vals : Validator[ F ]* ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( validators = fc.validators ++ vals.toSet ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withoutValidation : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( validators = Set.empty[ Validator[ F ] ] ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withDefault( defaultValue : F ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( default = Some( defaultValue ) ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] )
        def withoutDefault : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( default = None ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ F, N, S ] )
        def withExamples( exs : F* ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( examples = exs ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ F, N, S ] )
        def addExamples( exs : F* ) : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( examples = fc.examples ++ exs ) }
    extension [ T, F, N <: FieldName, S ]( field : Field.Aux[ F, N, S ] )
        def withoutExamples : Field.Aux[ T, F, N, S ] =
            field match { case fc : FieldCase[ T, F, N, S ] @unchecked => fc.copy[ T, F, N, S ]( examples = Nil ) }

    extension ( field : Field.type ) def builder[ F ] : FieldBuilderWithoutSchemaOrName[ F ] = FieldBuilder[ F ]

    class FromSchema[ T, F ] {
        def apply[ N <: FieldName ]( fieldName : N )( using sch : Schema[ F ] ) : Field.Aux[ T, F, N, sch.Shape ] = {
            FieldCase[ T, F, N, sch.Shape ]( fieldName, sch )
        }
    }

    extension ( field : Field.type ) def fromSchema[ T, F ] : FromSchema[ T, F ] = new FromSchema[ F ]

    class FromPrimitive[ T, F ] {
        def apply[ N <: FieldName ]( fieldName : N ) : Field.Aux[ T, F, N, Unit ] = {
            FieldCase[ T, F, N, Unit ]( fieldName, Primitive[ F ]() )
        }
    }

    extension ( field : Field.type ) def primitive[ T, F ] : FromPrimitive[ T, F ] = new FromPrimitive[ T, F ]

}
