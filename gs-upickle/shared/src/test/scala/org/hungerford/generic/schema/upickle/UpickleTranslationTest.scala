// package org.hungerford.generic.schema.upickle

// import org.hungerford.generic.schema.translation.{BiMapProductTranslationTest, SchemaTranslatorTest}
// import ujson.Value
// import upickle.default._

// import scala.collection.immutable.ListMap
// import scala.language.higherKinds

// class UPicklePrimitiveTranslationTest
//   extends SchemaTranslatorTest[ ReadWriter ]

// class UpickleProductTranslationTest
//   extends BiMapProductTranslationTest[ ReadWriter, Value.Value, ListMap[ String, Value.Value ] ]
//     with UPickleProductTranslation {

//     def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
// }
