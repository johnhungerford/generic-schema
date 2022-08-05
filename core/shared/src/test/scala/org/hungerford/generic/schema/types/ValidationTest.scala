package org.hungerford.generic.schema.types

import org.hungerford.generic.schema.utilities.ValidationDsl
import org.scalatest.flatspec.AnyFlatSpecLike

enum ValCopr:
    case IntC(int: Int)
    case StrC(str: String)

class ValidationTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    import generic.schema.exports.{*, given}
    import generic.schema.utilities.*

    behavior of "Primitive Validation"

    it should "validate using a primitive schema" in {
        val sch = Schema.primitive[Int].withValidation(Validator.max(25))
        import sch.givenSchema
        12.isValid shouldBe true
        25.isValid shouldBe true
        26.isValid shouldBe false
        100432.isValid shouldBe false
    }

    behavior of "Product Validation"

    case class ValProd(int: Int, str: String)

    it should "validate using a product schema's generic validators" in {
        val sch = Schema.derived[ValProd].withValidation(Validator(_.int <= 25), Validator(_.str.contains("lo")))
        import sch.givenSchema
        ValProd(12, "hello").isValid shouldBe true
        ValProd(12, "sdfs").isValid shouldBe false
        ValProd(26, "hello").isValid shouldBe false
    }

    it should "validate using a product schema's field validators" in {
        val sch = Schema.derived[ValProd]
          .modifyComponent("int")(_.withValidation(Validator.max(25)))
          .modifyComponent("str")(_.withValidation(Validator(_.contains("lo"))))
        import sch.givenSchema
        ValProd(12, "hello").isValid shouldBe true
        ValProd(12, "sdfs").isValid shouldBe false
        ValProd(26, "hello").isValid shouldBe false
    }

    it should "validate using a product schema's fields' schema's validation" in {
        val sch = Schema.derived[ValProd]
          .modifyComponent("int")(_.withSchema(Schema.primitive[Int].withValidation(Validator.max(25))))
          .modifyComponent("str")(_.withSchema(Schema.primitive[String].withValidation(Validator(_.contains("lo")))))
        import sch.givenSchema
        ValProd(12, "hello").isValid shouldBe true
        ValProd(12, "sdfs").isValid shouldBe false
        ValProd(26, "hello").isValid shouldBe false
    }

    it should "validate using generic validators, field validators, and field schema validators" in {
        val sch = Schema.derived[ValProd]
          .withValidation(Validator(v => if (v.int >= 25) v.str.length < 10 else true))
          .modifyComponent("int")(_.withValidation(Validator.max(100)))
          .modifyComponent("str")(_.withValidation(Validator(_.contains("lo"))))
          .modifyComponent("int")(_.withSchema(Schema.primitive[Int].withValidation(Validator.min(-100))))
          .modifyComponent("str")(_.withSchema(Schema.primitive[String].withValidation(Validator(!_.contains("hi")))))
        import sch.givenSchema
        ValProd(30, "hello").isValid shouldBe true
        ValProd(30, "abcdefghijklmnophello").isValid shouldBe false
        ValProd(120, "hello").isValid shouldBe false
        ValProd(99, "abcdef").isValid shouldBe false
        ValProd(-200, "lo").isValid shouldBe false
        ValProd(-99, "hilo").isValid shouldBe false
        ValProd(-100, "lo").isValid shouldBe true
        ValProd(100, "lo").isValid shouldBe true
    }

    behavior of "Coproduct Validation"

    it should "validate using schema's genericValidators" in {
        val sch = Schema.derived[ValCopr].withValidation(Validator {
            case ValCopr.IntC(int) => int <= 25
            case ValCopr.StrC(str) => str.contains("lo")
        })
        import sch.givenSchema
        (ValCopr.IntC(25): ValCopr).isValid shouldBe true
        (ValCopr.IntC(26): ValCopr).isValid shouldBe false
        (ValCopr.StrC("hello"): ValCopr).isValid shouldBe true
        (ValCopr.StrC("hi"): ValCopr).isValid shouldBe false
    }

    it should "validate using subtypes' validators" in {
        val sch = Schema.derived[ValCopr]
          .modifyComponent("IntC")(_.withValidation(Validator(_.int <= 25)))
          .modifyComponent("StrC")(_.withValidation(Validator(_.str.contains("lo"))))
        import sch.givenSchema
        (ValCopr.IntC(25): ValCopr).isValid shouldBe true
        (ValCopr.IntC(26): ValCopr).isValid shouldBe false
        (ValCopr.StrC("hello"): ValCopr).isValid shouldBe true
        (ValCopr.StrC("hi"): ValCopr).isValid shouldBe false
    }

    it should "validate using subtypes' schemas' validators" in {
        val sch = Schema.derived[ValCopr]
          .modifyComponent("IntC")(_.withSchema(Schema.derived[ValCopr.IntC].withValidation(Validator(_.int <= 25))))
          .modifyComponent("StrC")(_.withSchema(Schema.derived[ValCopr.StrC].withValidation(Validator(_.str.contains("lo")))))
        import sch.givenSchema
        (ValCopr.IntC(25): ValCopr).isValid shouldBe true
        (ValCopr.IntC(26): ValCopr).isValid shouldBe false
        (ValCopr.StrC("hello"): ValCopr).isValid shouldBe true
        (ValCopr.StrC("hi"): ValCopr).isValid shouldBe false
    }

    it should "validate using nested validation" in {
        val sch = Schema.derived[ValCopr]
          .modifyComponent("IntC" / "int")(_.withSchema(Schema.primitive[Int].withValidation(Validator.max(25))))
        import sch.givenSchema
        (ValCopr.IntC(25): ValCopr).isValid shouldBe true
        (ValCopr.IntC(26): ValCopr).isValid shouldBe false
    }

    it should "validated using generic, subtype, and subtype schema validators" in {
        val sch = Schema.derived[ValCopr]
          .withValidation(Validator {
                case ValCopr.IntC(int) => int <= 25
                case ValCopr.StrC(str) => str.contains("lo")
            })
          .modifyComponent("IntC")(
              _.withValidation(Validator(_.int > -100))
                .withSchema(Schema.derived[ValCopr.IntC].withValidation(Validator(_.int != 15)))
          )
          .modifyComponent("StrC")(
              _.withValidation(Validator(!_.str.contains("hi")))
                .withSchema(Schema.derived[ValCopr.StrC].withValidation(Validator(_.str != "yellow")))
          )
        import sch.givenSchema
        (ValCopr.IntC(13) : ValCopr).isValid shouldBe true
        (ValCopr.StrC("bellow"): ValCopr).isValid shouldBe true
        (ValCopr.IntC(26): ValCopr).isValid shouldBe false
        (ValCopr.IntC(-100): ValCopr).isValid shouldBe false
        (ValCopr.IntC(15): ValCopr).isValid shouldBe false
        (ValCopr.StrC("derp"): ValCopr).isValid shouldBe false
        (ValCopr.StrC("hilo"): ValCopr).isValid shouldBe false
        (ValCopr.StrC("yellow"): ValCopr).isValid shouldBe false
    }
}
