package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.{Schema, SchemaRebuilder, Primitive}
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.Field.Aux
import org.hungerford.generic.schema.validator.Validator

import scala.collection.mutable
import scala.language.higherKinds
import scala.annotation.meta.field
import scala.compiletime.{erasedValue, summonInline}
import org.hungerford.generic.schema.types.SimpleExtractor

type FieldName = Stringleton

sealed trait Field[ T ] {
    type Name <: FieldName
    type Shape

    def fieldName : Name
    def schema : Schema.Aux[ T, Shape ]
    def description : Option[ String ] = None
    def validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
    def default : Option[ T ]
    def examples : Seq[ T ]
    def deprecated : Boolean
}

case class FieldCase[ T, N <: FieldName, S ](
    override val fieldName : N,
    override val schema : Schema.Aux[ T, S ],
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    override val default : Option[ T ] = None,
    override val examples : Seq[ T ] = Nil,
    override val deprecated : Boolean = false,
) extends Field[ T ] {
    type Name = N
    type Shape = S
}

object Field {
    type Aux[ T, N <: FieldName, S ] = Field[ T ] { type Name = N; type Shape = S }
    type AuxN[ T, N <: FieldName ] = Field[ T ] { type Name = N }
    type AuxS[ T, S ] = Field[ T ] { type Shape = S }

    def apply[ T, N <: FieldName, S ](
        name : N,
        schema : Schema.Aux[ T, S ],
        description : Option[ String ] = None,
        validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    ) : Field.Aux[ T, N, S ] = FieldCase[ T, N, S ]( name, schema, description, validators )
}

trait FieldDsl {

    class FieldSchemaRebuilder[ Builder, T, N <: FieldName, S ]( builder : Builder, field : FieldCase[ T, N, S ] ) {
        def apply[ NewS ]( rebuilder : Builder => Schema.Aux[ T, NewS ] ) : Field.Aux[ T, N, NewS ] = {
            field.copy[ T, N, NewS ]( schema = rebuilder( builder ) )
        }
    }

    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def rebuildSchema( using rb : SchemaRebuilder[ T, S ] ) : FieldSchemaRebuilder[ rb.Builder, T, N, S ] =
            field match {
                case f : FieldCase[ T, N, S ] =>
                    new FieldSchemaRebuilder[ rb.Builder, T, N, S ]( rb.rebuild( f.schema ), f )
            }

    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def modifySchema[ NewS ](
            modifier : Schema.Aux[ T, S ] => Schema.Aux[ T, NewS ],
        ) : Field.Aux[ T, N, NewS ] = FieldCase[ T, N, NewS ](
            field.fieldName,
            modifier( field.schema ),
            field.description,
            field.validators,
        )

    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def rebuild : BuildableFieldBuilder[ T, N, S ] = FieldBuilder.from( field )

    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withName[ N1 <: FieldName ]( name : N1 ) : Field.Aux[ T, N1, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N1, S ]( fieldName = name ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withSchema[ S1 ]( sch : Schema.Aux[ T, S1 ] ) : Field.Aux[ T, N, S1 ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S1 ]( schema = sch ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withDescription( desc : String ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( description = Some( desc ) ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withoutDescription : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( description = None ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withValidators( vals : Validator[ T ]* ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( validators = vals.toSet ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def addValidators( vals : Validator[ T ]* ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( validators = fc.validators ++ vals.toSet ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withoutValidation : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( validators = Set.empty[ Validator[ T ] ] ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withDefault( defaultValue : T ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( default = Some( defaultValue ) ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withoutDefault : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( default = None ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withExamples( exs : T* ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( examples = exs ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def addExamples( exs : T* ) : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( examples = fc.examples ++ exs ) }
    extension [ T, N <: FieldName, S ]( field : Field.Aux[ T, N, S ] )
        def withoutExamples : Field.Aux[ T, N, S ] =
            field match { case fc : FieldCase[ T, N, S ] => fc.copy[ T, N, S ]( examples = Nil ) }

    extension ( field : Field.type ) def builder[ T ] : FieldBuilderWithoutSchemaOrName[ T ] = FieldBuilder[ T ]

    class FromSchema[ T ] {
        def apply[ N <: FieldName ]( fieldName : N )( using sch : Schema[ T ] ) : Field.Aux[ T, N, sch.Shape ] = {
            FieldCase[ T, N, sch.Shape ]( fieldName, sch )
        }
    }

    extension ( field : Field.type ) def fromSchema[ T ] : FromSchema[ T ] = new FromSchema[ T ]

    class FromPrimitive[ T ] {
        def apply[ N <: FieldName ]( fieldName : N ) : Field.Aux[ T, N, Unit ] = {
            FieldCase[ T, N, Unit ]( fieldName, Primitive[ T ]() )
        }
    }

    extension ( field : Field.type ) def primitive[ T ] : FromPrimitive[ T ] = new FromPrimitive[ T ]

}
