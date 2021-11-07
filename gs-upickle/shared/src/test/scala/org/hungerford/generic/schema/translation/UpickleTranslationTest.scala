package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaBuilder, SchemaDeriver, SchemaProvider}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.hungerford.generic.schema.upikle.UPickleProductTranslation
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import ujson.Value
import upickle.default._

import scala.collection.immutable.ListMap
import scala.language.higherKinds

class UPicklePrimitiveTranslationTest
  extends SchemaTranslatorTest[ ReadWriter ]

class UpickleProductTranslatorTest
  extends BiMapProductTranslatorTest[ ReadWriter, Value.Value, ListMap[ String, Value.Value ] ]
    with UPickleProductTranslation {

    def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
}
