package org.hungerford.generic.schema.circe

import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator, SingletonJsonTranslationTest}
import org.hungerford.generic.schema.Schema
import io.circe.*
import io.circe.parser.*

import CirceSchemaTranslation.given

class CirceSingletonSchemaTranslationTest
  extends SingletonJsonTranslationTest[ Codec ] {
    import SingletonJsonTranslationTest.*
    import org.hungerford.generic.schema.Default.dsl.*

    val tsOSchema: Codec[ TestSingleton.type ] = SchemaTranslator.translate( tsSchema )

    def stOSchema: Codec[ SuperT ] = SchemaTranslator.translate( stSchema )

    def writeJson[ T ]( value: T, schm: Codec[ T ] ): String = {
        given Codec[ T ] = schm
        schm( value ).noSpaces.toString
    }

    def readJson[ T ]( json: String, schm: Codec[ T ] ): Option[ T ] = {
        given Codec[ T ] = schm
        parse( json ).toOption flatMap { circeJson =>
            ( circeJson.as[ T ] ).toOption
        }
    }

}
