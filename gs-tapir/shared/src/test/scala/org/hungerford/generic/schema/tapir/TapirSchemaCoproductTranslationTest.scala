package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema
import org.scalatest.flatspec.AnyFlatSpecLike
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.Validator as TapirValidator
import org.hungerford.generic.schema.translation.SchemaTranslator
import sttp.tapir.SchemaType.{SCoproduct, SProduct, SString}

class TapirSchemaCoproductTranslationTest
  extends AnyFlatSpecLike
    with org.scalatest.matchers.should.Matchers
    with TapirSchemaCoproductTranslation
    with TapirSchemaProductTranslation
    with TapirSchemaSingletonTranslation {

    behavior of "TapirCoproductSchemaTranslation"

    sealed trait SuperT
    final case class SubT1( int : Int ) extends SuperT
    case object SubT2 extends SuperT

    it should "translate a coproduct schema into an SCoproduct schema type with the correct subtype schemas and correct subtype schema function" in {
        import org.hungerford.generic.schema.Default.dsl.*

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
                    case _ => fail( "first subtype had wrong schema" )
                }
                subtypes.tail.head.schemaType match {
                    case SString() =>
                    case _ => fail( "not a product!" )
                }
                subtypes.tail.head.validator match {
                    case TapirValidator.Enumeration( values, _, _ ) =>
                        values shouldBe List( SubT2 )
                    case _ => fail( "missing validation" )
                }
                sc.subtypeSchema( SubT2 ) match {
                    case Some( TapirSchema( SString(), _, _, _, _, _, _, _, TapirValidator.Enumeration( values, _, _ ) ) ) =>
                        values shouldBe List( SubT2 )
                    case Some( _ ) => fail( "not a singleton!" )
                    case None => fail( "couldn't find schema from value")
                }
                sc.subtypeSchema( SubT1( 234 ) ) match {
                    case Some( TapirSchema( SProduct( fields ), _, _, _, _, _, _, _, _ ) ) =>
                        fields.length shouldBe 1
                        fields.head.name.name shouldBe "int"
                    case Some( _ ) => fail( "not a product!" )
                    case None => fail( "couldn't find schema from value")
                }
            case _ => fail( "wrong schema type!" )
        }
    }

}