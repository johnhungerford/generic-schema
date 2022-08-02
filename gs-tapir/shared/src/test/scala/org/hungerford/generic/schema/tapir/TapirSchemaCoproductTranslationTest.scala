package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema
import org.scalatest.flatspec.AnyFlatSpecLike
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.Validator as TapirValidator
import sttp.tapir.SchemaType.SchemaWithValue
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.types.ExistsFor
import sttp.tapir.SchemaType.{SCoproduct, SProduct, SRef, SString}

class TapirSchemaCoproductTranslationTest
  extends AnyFlatSpecLike
    with org.scalatest.matchers.should.Matchers
    with TapirSchemaCoproductTranslation
    with TapirSchemaProductTranslation
    with TapirSchemaSingletonTranslation {

    behavior of "TapirCoproductSchemaTranslation"

    sealed trait SuperT
    final case class SubT1( int : Int ) extends SuperT
    final case class SubT2( str : String ) extends SuperT

    it should "translate a coproduct schema into an SCoproduct schema type with the correct subtype schemas and correct subtype schema function" in {
        import generic.schema.exports.*

        val sch = Schema.derived[ SuperT ]

        val tapirSch : TapirSchema[ SuperT ] = SchemaTranslator.translate( sch )

        tapirSch.schemaType match {
            case sc@SCoproduct( subtypes, discriminator ) =>
                discriminator shouldBe None
                subtypes.length shouldBe 2
                subtypes.head.schemaType match {
                    case SProduct( fields ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "int"
                    case _ =>
                        println( tapirSch )
                        fail( "first subtype had wrong schema" )
                }
                subtypes.tail.head.schemaType match {
                    case SProduct( fields ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "str"
                    case _ =>
                        println( tapirSch )
                        fail( "second subtype had wrong schema" )
                }
                sc.subtypeSchema( SubT2( "hello" ) ) match {
                    case Some( SchemaWithValue( TapirSchema( SProduct( fields ), _, _, _, _, _, _, _, _, _, _ ), _ ) ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "str"
                    case Some( _ ) => fail( "not a product!" )
                    case None => fail( "couldn't find schema from value")
                }
                sc.subtypeSchema( SubT1( 234 ) ) match {
                    case Some( SchemaWithValue( TapirSchema( SProduct( fields ), _, _, _, _, _, _, _, _, _, _ ), _ ) ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "int"
                    case Some( _ ) => fail( "not a product!" )
                    case None => fail( "couldn't find schema from value")
                }
            case _ => fail( "wrong schema type!" )
        }
    }

    sealed trait StringEnum
    case object Val1 extends StringEnum
    case object Val2 extends StringEnum
    case object Val3 extends StringEnum

    it should "translate a sealed trait of case objects as a single string enum" in {
        import generic.schema.exports.*

        val sch = Schema.derived[ StringEnum ]

        val sts = sch.shape.subtypeDescriptions

        summon[ ExistsFor[ IsSingletonSubtype, sts.type ] ]

        val tapirSch : TapirSchema[ StringEnum ] = SchemaTranslator.translate( sch )

        tapirSch.schemaType match {
            case SString() =>
            case _ =>
                fail( "Not a string type" )
        }

        tapirSch.validator match {
            case TapirValidator.Enumeration( values, Some( encoder ), _ ) =>
                values shouldBe List( Val1, Val2, Val3 )
                encoder( Val1 ) shouldBe Some( "Val1" )
                encoder( Val2 ) shouldBe Some( "Val2" )
                encoder( Val3 ) shouldBe Some( "Val3" )
            case _ => fail( "validator is not enum" )
        }
    }

    sealed trait MixedCoproduct
    final case class MC1( int : Int ) extends MixedCoproduct
    case object MC2 extends MixedCoproduct
    final case class MC3( str : String ) extends MixedCoproduct
    case object MC4 extends MixedCoproduct

    it should "translate a with singleton and non-singleton subtypes to an SCoproduct schema type with a single string enum for all singleton subtypes and subsequently the other types in order" in {
        import generic.schema.exports.*

        val sch = Schema.derived[ MixedCoproduct ]

        val tapirSch : TapirSchema[ MixedCoproduct ] = SchemaTranslator.translate( sch )

        tapirSch.schemaType match {
            case sc@SCoproduct( subtypes, discriminator ) =>
                discriminator shouldBe None
                subtypes.length shouldBe 3
                subtypes.head.schemaType match {
                    case SString() =>
                        subtypes.head.validator match {
                            case TapirValidator.Enumeration( values, _, _ ) =>
                                values shouldBe List( "MC2", "MC4" )
                            case _ => fail( "enum string schema had no enum validation" )
                        }
                    case _ =>
                        println( tapirSch )
                        fail( "first subtype was not a string schema" )
                }
                subtypes.tail.head.schemaType match {
                    case SProduct( fields ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "int"
                    case _ =>
                        println( tapirSch )
                        fail( "second subtype had wrong schema" )
                }
                subtypes.tail.tail.head.schemaType match {
                    case SProduct( fields ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "str"
                    case _ =>
                        println( tapirSch )
                        fail( "second subtype had wrong schema" )
                }
                sc.subtypeSchema( MC1( 234 ) ) match {
                    case Some( SchemaWithValue( TapirSchema( SProduct( fields ), _, _, _, _, _, _, _, _, _, _ ), _ ) ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "int"
                    case Some( _ ) => fail( "not a product!" )
                    case None => fail( "couldn't find schema from value")
                }
                sc.subtypeSchema( MC3( "hello" ) ) match {
                    case Some( SchemaWithValue( TapirSchema( SProduct( fields ), _, _, _, _, _, _, _, _, _, _ ), _ ) ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "str"
                    case Some( _ ) => fail( "not a product!" )
                    case None => fail( "couldn't find schema from value")
                }

            case _ => fail( "wrong schema type!" )
        }
    }

    sealed trait Recur
    case object Terminal extends Recur
    final case class RecurOnce( a : Int, b : Recur ) extends Recur

    it should "be able to translate a recursive schema" in {
        import generic.schema.exports.*

        val sch = Schema.derived[ Recur ]

        val tapirSch : TapirSchema[ Recur ] = SchemaTranslator.translate( sch )

        tapirSch.schemaType match {
            case sc@SCoproduct( subtypes, discriminator ) =>
                discriminator shouldBe None
                subtypes.length shouldBe 2
                subtypes.head.schemaType match {
                    case SString() =>
                    case _ =>
                        println( tapirSch )
                        fail( "first subtype was not a string schema" )
                }
                subtypes.tail.head.schemaType match {
                    case SProduct( fields ) =>
                        fields.length shouldBe 2
                        fields.head.name.name shouldBe "a"
                        fields.tail.head.name.name shouldBe "b"
                        fields.tail.head.schema.schemaType match {
                            case SRef( TapirSchema.SName( "Recur", Nil ) ) =>
                            case _ =>
                                println( tapirSch )
                                fail( s"Not the correct schematype: ${fields.tail.head.schema.schemaType}" )
                        }
                    case _ =>
                        println( tapirSch )
                        fail( "second subtype had wrong schema" )
                }

            case _ => fail( "wrong schema type!" )
        }
    }

}
