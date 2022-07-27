package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.translation.{CI, RecursiveSchemaTranslator, SchemaTranslator}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.Default.dsl.*
import sttp.tapir
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.Schema.SName
import sttp.tapir.SchemaType.{SCoproduct, SInteger, SProduct, SProductField, SRef}

class TapirSchemaProductTranslationTest
  extends AnyFlatSpecLike
    with Matchers
    with TapirSchemaCoproductTranslation
    with TapirSchemaProductTranslation
    with TapirSchemaSingletonTranslation {

    behavior of "TapirSchemaProductTranslation"

    case class TestCase( int : Int, str : String, bool : Boolean )

    it should "translate a product schema into an SProduct schema type with the correct field names, correct getters, correct validators, and the correct description" in {
        val sch = Schema.derived[ TestCase ]
          .withDescription( "test-case-description" )
          .modifyComponent( "int" )(
              _.addValidators(
                  Validator.min[ Int ]( -5 ),
                  Validator.maxExclusive[ Int ]( 25 ),
                  Validator.nonZero[ Int ],
              )
          )

        val tapirSchema : TapirSchema[ TestCase ] = SchemaTranslator.translate( sch )

        val tc = TestCase( 5, "hello", true )

        tapirSchema.schemaType match {
            case SProduct( fields : List[ SProductField[ TestCase ] ] ) =>
                fields.size shouldBe 3
                fields.map( _.name.name ).toSet shouldBe Set( "int", "str", "bool" )
                fields.find( _.name.name == "int" ).get.get( tc ) shouldBe Some( 5 )
                fields.find( _.name.name == "str" ).get.get( tc ) shouldBe Some( "hello" )
                fields.find( _.name.name == "bool" ).get.get( tc ) shouldBe Some( true )
                val intValidator : tapir.Validator[ Int ] =
                    fields.find( _.name.name == "int" ).get.schema.validator.asInstanceOf[ tapir.Validator[ Int ] ]
                intValidator( -6 ).isEmpty shouldBe false
                intValidator( -5 ).isEmpty shouldBe true
                intValidator( -1 ).isEmpty shouldBe true
                intValidator( 0 ).isEmpty shouldBe false
                intValidator( 1 ).isEmpty shouldBe true
                intValidator( 10 ).isEmpty shouldBe true
                intValidator( 24 ).isEmpty shouldBe true
                intValidator( 25 ).isEmpty shouldBe false
                intValidator( 26 ).isEmpty shouldBe false

            case _ => fail( "Product schema did not generate an SProduct tapir schema type" )
        }

        tapirSchema.description should contain( "test-case-description" )
    }

    sealed trait Recur
    case object Terminal extends Recur
    final case class RecurOnce( a : Int, b : Recur ) extends Recur

    it should "translate a recursive product correctly" in {

//        val sch = Schema.derived[ RecurOnce ]
//        val recurSch = sch( 1 ).schema

//        RecursiveSchemaTranslator.translate[ Recur, recurSch.Shape, CI[ Recur, String ] *: EmptyTuple, TapirSchema ]( recurSch, CI.of[ Recur ]( "TYPENAME" ) *: EmptyTuple )

        val sch = Schema.derived[ RecurOnce ]
        val tapirSch : TapirSchema[ RecurOnce ] = SchemaTranslator.translate( sch )

        tapirSch.schemaType match {
            case SProduct( fields : List[ SProductField[ TestCase ] ] ) =>
                fields.size shouldBe 2
                fields.head.name.name shouldBe "a"
                fields.head.schema.schemaType match {
                    case SInteger() =>
                    case other =>
                        fail( s"RecurOnce field \"a\" is not integer: ${other}" )
                }
                fields.tail.head.name.name shouldBe "b"
                fields.tail.head.schema.schemaType match {
                    case SCoproduct( subtypes, _ ) =>
                        subtypes.size shouldBe 2
                        subtypes.tail.head.name should contain( SName( "RecurOnce" ) )
                        subtypes.tail.head.schemaType match {
                            case SRef( name ) =>
                                name shouldBe SName( "RecurOnce" )
                            case other =>
                                fail( s"wrong schema type (should be SRef): ${other}" )
                        }
                }

            case _ => fail( "Product schema did not generate an SProduct tapir schema type" )
        }
    }

    it should "translate complex recursive schemas" in {
        import org.hungerford.generic.schema.TestADTs.Recursive.*

        val complexSch = Schema.derived[ Outer ]
        val complexTapirSch = complexSch.as[ tapir.Schema ]
    }

}
