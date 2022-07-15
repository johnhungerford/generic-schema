package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.SchemaProvider
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.coproduct.translation.BiMapCoproductTranslation
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.field.FieldName

import scala.util.{Failure, Success, Try}
import ujson.Value
import upickle.default.{read as upkRead, write as upkWrite, *}

trait UPickleCoproductSchemaTranslation extends BiMapCoproductTranslation[ ReadWriter, Reader, Writer, Value.Value, Value.Value ] {

    def buildCoproductSchema[ T ]( enc : Writer[ T ], dec : Reader[ T ] ) : ReadWriter[ T ] =
        ReadWriter.join( dec, enc )

    def buildCoproductDecoder[ T ](
        decode: Value.Value => Option[ T ]
    ): Reader[ T ] = reader[ Value.Value ].map( v => decode( v ).get )

    def buildCoproductEncoder[ T ]( encode: T => Value.Value ): Writer[ T ] =
        readwriter[ Value.Value ].bimap[ T ](encode, _.asInstanceOf[ T ] )

    given [ ST, N <: TypeName ] : SubtypeReader[ ST, N ] with {
        def read( from : Value.Value, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : Reader[ ST ] ) : Option[ ST ] =
            given Reader[ ST ] = schema
            Try( upickle.default.read[ ST ]( from ) ).toOption
    }

    given [ DV ] : DiscrReader[ DV ] with {
        def read( from : Value.Value, name : String, schema : Reader[ DV ] ) : Option[ DV ] =
            given Reader[ DV ] = schema
            Try( from( name ) ).toOption flatMap { newValue =>
                Try( upickle.default.read[ DV ]( newValue ) ).toOption
            }
    }

    given [ T, ST, N <: TypeName ] : SubtypeWriter[ ST, N ] with {
        def write( value : ST, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : Writer[ ST ] ) : Value.Value =
            given Writer[ ST ] = schema
            upickle.default.writeJs[ ST ]( value )
    }

}

object UPickleCoproductSchemaTranslation extends UPickleCoproductSchemaTranslation
