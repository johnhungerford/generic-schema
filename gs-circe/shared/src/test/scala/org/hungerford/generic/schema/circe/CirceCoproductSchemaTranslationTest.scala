package org.hungerford.generic.schema.circe

import io.circe.*
import io.circe.syntax.*
import org.hungerford.generic.schema.translation.{CoproductJsonTranslationTest, SchemaTranslator, Super, SuperT}

import CirceSchemaTranslation.given

class CirceCoproductSchemaTranslationTest
  extends CoproductJsonTranslationTest[ Codec ] {

    override val superOs: Codec[ Super ] = SchemaTranslator.translate( superSch )
    override def superTOs: Codec[ SuperT ] = SchemaTranslator.translate( superTSch )

    override def writeJson[ T ]( value: T, schm: Codec[ T ] ): String = {
            given Codec[ T ] = schm
            value.asJson.noSpaces.toString
    }

}
