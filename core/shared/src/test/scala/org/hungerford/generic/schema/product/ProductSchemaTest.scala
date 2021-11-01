//package org.hungerford.generic.schema.product
//
//import org.hungerford.generic.schema.bridge.{AdditionalFieldsExtractor, FieldWriter, ProductSchemaBridge, UjsonSchemaBridge}
//import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, FieldTranslator, TranslatedFieldDescription}
//import org.hungerford.generic.schema.types.{Extractor, SimpleExtractor}
//import org.hungerford.generic.schema.{NoSchema, Primitive}
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.scalatest.matchers.should.Matchers
//import shapeless._
//import shapeless.ops.hlist.Mapper
//import shapeless.poly._
//import ujson.Value
//import upickle.default
//import upickle.default._
//
//import scala.util.Try
//
//class ProductSchemaTest extends AnyFlatSpecLike with Matchers {
//
//    behavior of "ProductSchema"
//
//    it should "construct a simple product schema" in {
//        case class SimpleCase( i : Int, str : String )
//
//        type SCRV = Int :: String :: HNil
//        type SCR = FieldDescription[ Int ] :: FieldDescription[ String ] :: HNil
//        type TSCR = TranslatedFieldDescription[ Int, ReadWriter ] :: TranslatedFieldDescription[ String, ReadWriter ] :: HNil
//        type PS = ProductSchema[ SimpleCase, SCR, SCRV, Int, Nothing ]
//        type PSBridge = ProductSchemaBridge[ SimpleCase, SCR, SCRV, Int, Nothing, ReadWriter ]
//
//        implicit val schema = ProductSchema[ SimpleCase, SCR, SCRV, Int, Nothing ](
//            genericDescription = Some( "test description" ),
//            genericValidators = Set.empty,
//            fieldDescriptions = FieldDescriptionCase[ Int, Nothing ]( "int_field", Primitive[ Int ]() ) ::  FieldDescriptionCase[ String, Nothing ]( "str_field", Primitive[ String ]() ) :: HNil,
//            additionalFieldsSchema = Primitive[ Int ](),
//            constructor = (rvt, _) => implicitly[ Generic.Aux[ SimpleCase, Int :: String :: HNil ] ].from( rvt ),
//            deconstructor = ( value : SimpleCase ) => (implicitly[ Generic.Aux[ SimpleCase, Int :: String :: HNil ] ].to( value ), Map.empty)
//        )
//
//        import org.hungerford.generic.schema.bridge.UjsonSchemaBridge._
//
//
//        implicit def afeInt : AdditionalFieldsExtractor[ Int, Value ] = new AdditionalFieldsExtractor[ Int, Value.Value ] {
//            override def extract( from : Value ) : Map[ String, Int ] = from.obj.toMap collect {
//                case (fieldName, value : Value) if Try( value.num.toInt ).toOption.nonEmpty => (fieldName -> value.num.toInt)
//            }
//        }
//
//        implicit def fwInt : FieldWriter[ Int, Map[String, Value ], default.ReadWriter ] = new FieldWriter[ Int, Map[ String, Value.Value ], ReadWriter ] {
//            override def write(
//                field : Int,
//                to : Map[ String, Value ],
//                using : TranslatedFieldDescription[ Int, default.ReadWriter ] ) : Map[ String, Value ] = to + (using.fieldName -> ujson.Value.JsonableInt( field ) )
//        }
//
//        implicit def fwStr : FieldWriter[ String, Map[String, Value ], default.ReadWriter ] = new FieldWriter[ String, Map[ String, Value.Value ], ReadWriter ] {
//            override def write(
//                field : String,
//                to : Map[ String, Value ],
//                using : TranslatedFieldDescription[ String, default.ReadWriter ] ) : Map[ String, Value ] = to + (using.fieldName -> ujson.Value.JsonableString( field ) )
//        }
//
//        implicit def ujsonSE[ T ] : SimpleExtractor.Aux[ Value.Value, TranslatedFieldDescription[ T, ReadWriter ], T ] = {
//            new SimpleExtractor[ Value.Value, TranslatedFieldDescription[ T , ReadWriter ] ] {
//                override type Res = T
//
//                override def extract( from : Value, using : TranslatedFieldDescription[ T, default.ReadWriter ] ) : Res = {
//                    read[ T ]( from.obj( using.fieldName ) )( using.schema )
//                }
//            }
//        }
//
//        implicitly[ Extractor.Aux[ Value.Value, HNil, TSCR, SCRV ] ]
//
//        implicitly[ FieldTranslator[ String, ReadWriter ] ]
//
////        implicit def ujsonTrans[ T ] : FieldTranslator[ T]
//
//        import RWFieldDescriptionMapper._
//
//        implicitly[ Mapper.Aux[ RWFieldDescriptionMapper.type, FieldDescription[ String ] :: HNil, TranslatedFieldDescription[ String, ReadWriter ] :: HNil ] ]
//
//        implicitly[ Mapper.Aux[ RWFieldDescriptionMapper.type, SCR, TSCR ] ]
//
//        UjsonSchemaBridge.productTranslation[ SimpleCase, SCR, SCRV, TSCR, Int ]
//
//        implicit val simpleCaseUjsonRW : default.ReadWriter[ SimpleCase ] = implicitly[ ProductSchemaBridge[ SimpleCase, SCR, SCRV, Int, Nothing, ReadWriter ] ]
//          .translate( schema )
//
//        println( write( SimpleCase( 5, "hello" ) ) )
//    }
//
//}
