package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.translation.{CoproductJsonTranslationTest, ProductJsonTranslationTest, SchemaTranslator, PrimitiveSchemaTranslatorTest, SingletonJsonTranslationTest}
import ujson.Value
import upickle.default.*

import scala.collection.immutable.ListMap
import scala.util.Try

import UPickleSchemaTranslation.given

class UPicklePrimitiveTranslationTest
  extends PrimitiveSchemaTranslatorTest[ ReadWriter ]

class UpickleProductTranslationTest
  extends ProductJsonTranslationTest[ ReadWriter ] {

    def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
}

class UPickleCoproductSchemaTranslationTest
  extends CoproductJsonTranslationTest[ ReadWriter ] {
    import org.hungerford.generic.schema.translation.{Super, SuperT}

    override val superOs: ReadWriter[ Super ] = SchemaTranslator.translate( superSch )
    override def superTOs: ReadWriter[ SuperT ] = SchemaTranslator.translate( superTSch )

    override def writeJson[ T ]( value: T, schm: ReadWriter[ T ] ): String = write[ T ]( value )( schm )
}

class UPickleSingletonSchemaTranslationTest
  extends SingletonJsonTranslationTest[ ReadWriter ] {
    import SingletonJsonTranslationTest.*

    val tsOSchema: ReadWriter[ TestSingleton.type ] = SchemaTranslator.translate( tsSchema )
    val stOSchema: ReadWriter[ SuperT ] = SchemaTranslator.translate( stSchema )

    def writeJson[ T ]( value: T, schm: ReadWriter[ T ] ): String = {
        write[ T ]( value )( schm )
    }

    def readJson[ T ]( json: String, schm: ReadWriter[ T ] ): Option[ T ] = {
        read[ T ]( json )( schm )
        Try( read[ T ]( json )( schm ) ).toOption
    }
}
