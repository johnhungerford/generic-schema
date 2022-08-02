package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.product.field.FieldBuilder
import org.scalatest.flatspec.AnyFlatSpecLike

sealed trait CoprA
case object SubtA1 extends CoprA
case object SubtA2 extends CoprA
case class SubtA3(a: Int, b: Double) extends CoprA

sealed trait CoprB
case object SubtB1 extends CoprB
case object SubtB2 extends CoprB
case class SubtB3(e: Int, f: Double) extends CoprB

sealed trait RecCoprA
case object RecCoprTermA extends RecCoprA
case class InnerA(a: Int, b: ExternalA) extends RecCoprA
sealed trait ExternalA
case object ExtTermA extends ExternalA
case class RecProdA(inner: InnerA) extends ExternalA

sealed trait RecCoprB
case object RecCoprTermB extends RecCoprB
case class InnerB(a: Int, b: ExternalB) extends RecCoprB
sealed trait ExternalB
case object ExtTermB extends ExternalB
case class RecProdB(inner: InnerB) extends ExternalB

class ShapeIsomorphismTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Product Isomorphism"

    case class Prod1(a: Int, b: Boolean, c: Double)
    case class Prod2(d: Int, e: Boolean, f: Double)

    val p1Sch = {
        import generic.schema.exports.{*, given}
        Schema.derived[Prod1]
    }
    val p2Sch = {
        import generic.schema.exports.{*, given}
        Schema.derived[Prod2]
    }

    it should "convert a primitive to itself" in {
       import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        val iso = summon[Isomorphism[Int, Int]]
        iso.convertForward(23) shouldBe 23
        iso.convertBackward(23) shouldBe 23
    }

    it should "use a field to convert a type" in {
        import generic.schema.exports.{*, given}

        case object T1
        case object T2

        val field1 = FieldBuilder[ String, T1.type ].name("T1").extractor(_ => T1).singleton(T1, "T1").build
        val field2 = FieldBuilder[ Int, T2.type ].name("T2").extractor(_ => T2).singleton(T2, "T2").description("description").build

        val f1Sch = field1.schema
        val f2Sch = field2.schema

        type F1S = f1Sch.Shape
        type F2S = f2Sch.Shape

        val iso = summon[ShapeIsomorphism.Aux[T1.type, F1S, T1.type, T2.type, F2S, T2.type ]]
        iso.convertForward(T1, f1Sch.shape, f2Sch.shape) shouldBe T2
        iso.convertBackward(T2, f1Sch.shape, f2Sch.shape) shouldBe T1
    }

    it should "convert a case class instance to another case class of the same shape using product shape with no additional fields" in {
        import p1Sch.givenSchema
        import p2Sch.givenSchema

        val iso = summon[Isomorphism[Prod1, Prod2]]
        iso.convertForward(Prod1(23, false, 0.23423)) shouldBe Prod2(23, false, 0.23423)
        iso.convertBackward(Prod2(23, false, 0.23423)) shouldBe Prod1(23, false, 0.23423)
    }

    case class AFPr1(a: Map[String, Int], b: String, c: Boolean)
    case class AFPr2(d: String, e: Boolean, f: Map[String, Int])

    it should "convert a case class instance to another case class of the same shape using product shape with additional fields, even if additional fields are in different places" in {
        val (afSch1, afSch2) = {
            import generic.schema.exports.{*, given}
            val afs1 = Schema.productBuilder[AFPr1]
              .additionalFields[Int].primitive(_.a)
              .buildField[String](_.name("b").extractor(_.b).primitive.build)
              .buildField[Boolean](_.name("c").extractor(_.c).primitive.build)
              .construct({ case ((b, c), af) => AFPr1(af, b, c) })
              .build

            val afs2 = Schema.productBuilder[AFPr2]
              .buildField[String](_.name("d").extractor(_.d).primitive.build)
              .buildField[Boolean](_.name("e").extractor(_.e).primitive.build)
              .additionalFields[Int].primitive(_.f)
              .construct({ case ((b, c), af) => AFPr2(b, c, af) })
              .build

            (afs1, afs2)
        }

        import afSch1.givenSchema
        import afSch2.givenSchema

        val iso = summon[Isomorphism[AFPr1, AFPr2]]
        iso.convertForward(AFPr1(Map("hi" -> 23), "what", false)) shouldBe AFPr2("what", false, Map("hi" -> 23))
        iso.convertBackward(AFPr2("what", false, Map("hi" -> 23))) shouldBe AFPr1(Map("hi" -> 23), "what", false)
    }

    behavior of "Coproduct Isomorphism"

    it should "convert a coproduct" in {
        val (cprASch, cprBSch) = {
            import generic.schema.exports.*
            val schA = Schema.derived[CoprA]
            val schB = Schema.derived[CoprB]
            (schA, schB)
        }

        import cprASch.givenSchema
        import cprBSch.givenSchema
        val iso = summon[Isomorphism[CoprA, CoprB]]
        iso.convertForward(SubtA1) shouldBe SubtB1
        iso.convertForward(SubtA2) shouldBe SubtB2
        iso.convertBackward(SubtB1) shouldBe SubtA1
        iso.convertBackward(SubtB2) shouldBe SubtA2
        iso.convertForward(SubtA3(5, 0.232)) shouldBe SubtB3(5, 0.232)
        iso.convertBackward(SubtB3(5, 0.232)) shouldBe SubtA3(5, 0.232)
    }

    behavior of "Recursive Isomorphism"

    it should "be able to convert to and from recursive, isomorphic data types" in {
        val (recSchA, recSchB) = {
            import generic.schema.exports.*
            (Schema.derived[RecCoprA], Schema.derived[RecCoprB])
        }

        import recSchA.givenSchema, recSchB.givenSchema

        import generic.schema.utilities.*
        val coprA : RecCoprA = InnerA(23, ExtTermA)
        coprA.convert[RecCoprB] shouldBe InnerB(23, ExtTermB)
    }

}
