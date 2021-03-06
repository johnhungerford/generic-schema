package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.SchemaTranslator
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.SchemaType
import sttp.tapir.Validator as TapirValidator

trait TapirSchemaSingletonTranslation {

    // Default translation of singleton is string with enum validator restring to a single value
    given [ T <: Singleton, N <: TypeName ] : SchemaTranslator[ T, SingletonShape[ T, N ], TapirSchema ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ]
        ) : TapirSchema[ T ] = TapirSchema(
            schemaType = SchemaType.SString(),
            name = schema.name.map( n => TapirSchema.SName( n ) ),
            isOptional = false,
            description = schema.genericDescription,
            encodedExample = schema.genericExamples.headOption,
            deprecated = schema.deprecated,
            validator = TapirValidator.enumeration( List( schema.shape.value ), v => Some( schema.shape.name ) ),
        )
    }

}
