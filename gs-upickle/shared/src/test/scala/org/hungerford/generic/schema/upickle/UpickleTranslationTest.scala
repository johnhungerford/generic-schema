package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.translation.{CoproductJsonTranslationTest, ProductJsonTranslationTest, SchemaTranslator, SchemaTranslatorTest, SuperT, Super}
import ujson.Value
import upickle.default.*

import scala.collection.immutable.ListMap
import UPickleSchemaTranslation.given

class UPicklePrimitiveTranslationTest
  extends SchemaTranslatorTest[ ReadWriter ]

class UpickleProductTranslationTest
  extends ProductJsonTranslationTest[ ReadWriter ] {

    def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
}

class UPickleCoproductSchema
  extends CoproductJsonTranslationTest[ ReadWriter ] {

    override val superOs: ReadWriter[ Super ] = SchemaTranslator.translate( superSch )
    override def superTOs: ReadWriter[ SuperT ] = SchemaTranslator.translate( superTSch )

    override def writeJson[ T ]( value: T, schm: ReadWriter[ T ] ): String = write[ T ]( value )( schm )

}
