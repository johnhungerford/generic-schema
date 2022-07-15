package org.hungerford.generic.schema.circe

import io.circe.*
import io.circe.syntax.*
import org.hungerford.generic.schema.translation.{CoproductJsonTranslationTest, Rec, SchemaTranslator, Super, SuperT}
import CirceSchemaTranslation.given

class CirceCoproductSchemaTranslationTest
  extends CoproductJsonTranslationTest[ Codec ] {

    override val superOs: Codec[ Super ] = SchemaTranslator.translate( superSch )
    override def superTOs: Codec[ SuperT ] = SchemaTranslator.translate( superTSch )

    import recurSch.givenSchema

    override val recurSchOs: Codec[ Rec ] = SchemaTranslator.translate( recurSch )

    override def writeJson[ T ]( value: T, schm: Codec[ T ] ): String = {
            given Codec[ T ] = schm
            value.asJson.noSpaces.toString
    }

    override def readJson[ T ]( value: String, schm: Codec[ T ] ): T = {
        import io.circe.parser.decode

        given Codec[ T ] = schm
        decode[ T ]( value ).toTry.get
    }



}
