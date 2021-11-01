package org.hungerford.generic.schema.bridge

import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, TranslatedFieldDescription}
import org.hungerford.generic.schema.types.{Injector, SimpleExtractor}
import org.hungerford.generic.schema.{NoSchema, Primitive}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import ujson.Value
import upickle.default
import upickle.default._

class BiMapProductSchemaBridgeTest extends AnyFlatSpecLike with Matchers {

    case class NoAF( intField : Int, strField : String )

    type RVt = Int :: String :: HNil
    type Rt = FieldDescription.Aux[ Int, Nothing ] :: FieldDescription.Aux[ String, Nothing ] :: HNil
    type FDL = TranslatedFieldDescription[ Int, ReadWriter ] :: TranslatedFieldDescription[ String, ReadWriter ] :: HNil

    behavior of "BiMapProductSchemaBridge.Implicits.productTranslationWithoutAF"

    val intFieldDesc : FieldDescription.Aux[ Int, Nothing ] = FieldDescriptionCase[ Int, Nothing ]( "int_field", Primitive[ Int ]() )
    val strFieldDesc : FieldDescription.Aux[ String, Nothing ] = FieldDescriptionCase[ String, Nothing ]( "str_field", Primitive[ String ]() )

    it should "translate a product schema without additional fields" in {

        val generic = Generic.materialize[ NoAF, RVt ]

        implicit val testSchema = ProductSchema[ NoAF, FieldDescription.Aux[ Int, Nothing ] :: FieldDescription.Aux[ String, Nothing ] :: HNil, Int :: String :: HNil, Nothing, Nothing ](
            Some( "test schema description" ),
            Set.empty,
            intFieldDesc :: strFieldDesc :: HNil,
            NoSchema,
            ( rvt, _ ) => generic.from( rvt ),
            ( value ) => (generic.to( value ), Map.empty),
        )

        val upickleTransl = new BiMapProductSchemaBridge[ ReadWriter, Value.Value, Map[ String, Value.Value ] ] {

            /**
             * Construct a schema from the two parts of a bimap.
             *
             * @param to   T => MapVal : writer
             * @param from MapVal => T : reader
             * @tparam T type being read/written
             * @return type class instance for some reader/writer
             */
            override def schemaFromBimap[ T ]( to : T => Value, from : Value => T ) : ReadWriter[ T ] = {
                readwriter[ Value.Value ].bimap[ T ]( to, from )
            }

            /**
             * Initial empty value for the type being mapped to and from, that can be built
             * by adding field values. For instance, if the value type is Map[ String, T ],
             * initMapVal would be Map.empty[ String, T ]
             *
             * @return initial value of buildable bimap type
             */
            override def initMapVal : Map[ String, Value ] = Map.empty

            /**
             * Construct the final type to be bimapped to and from
             *
             * @param buildableValue
             * @return
             */
            override def buildMapVal( buildableValue : Map[ String, Value ] ) : Value = {
                val bvSeq : Seq[ (String, Value) ] = buildableValue.toSeq
                ujson.Obj( bvSeq.head, bvSeq : _* )
            }

            override def extractField[ T ](
                from : Value,
                using : TranslatedFieldDescription[ T, default.ReadWriter ],
            ) : T = {
                val fieldValue : Value = from.obj( using.fieldName )
                val rw : ReadWriter[ T ] = using.schema
                read[ T ]( fieldValue )( rw )
            }

            override def extractAdditionalFields[ T ]( from : Value, using : default.ReadWriter[ T ] ) : Map[String, T ] = ???

            override def writeField[ T ]( value : T, to : Map[String, Value ], using : TranslatedFieldDescription[ T, default.ReadWriter ] ) : Map[String, Value ] = {
                val valueJson : Value.Value = writeJs( value )( using.schema )
                to + ((using.fieldName, valueJson ))
            }

            override def writeAdditionalFields[ T ]( from : Map[String, T ], to : Map[String, Value ], using : default.ReadWriter[ T ] ) : Map[String, Value ] = ???
        }

        import upickleTransl.Implicits._

        implicit val noAfRw: ReadWriter[ NoAF ] = rw

        write( NoAF( 1, "hello" ) ) shouldBe """{"int_field":1,"str_field":"hello"}"""
    }

}
