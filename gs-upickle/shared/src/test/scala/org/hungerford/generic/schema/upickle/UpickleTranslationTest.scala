package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.translation.{ProductTranslationTest, SchemaTranslatorTest}
import ujson.Value
import upickle.default._

import scala.collection.immutable.ListMap
import scala.language.higherKinds

class UPicklePrimitiveTranslationTest
  extends SchemaTranslatorTest[ ReadWriter ]

import UPickleProductTranslation.given

class UpickleProductTranslationTest
  extends ProductTranslationTest[ ReadWriter ] {

    def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
}
