package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype}
import org.hungerford.generic.schema.types.Zipper
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.singleton.SingletonShape
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.deriving.Mirror

class CoproductDeriverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    sealed trait SuperT
    final case class SubT1( int : Int ) extends SuperT
    final case class SubT2( str : String ) extends SuperT

    behavior of "CoproductDeriver"

    it should "derive a simple coproduct" in {
        val mir = summon[Mirror.SumOf[SuperT]]

        summon[ mir.MirroredElemTypes =:= (SubT1, SubT2) ]
        summon[ mir.MirroredElemLabels =:= ("SubT1", "SubT2") ]

        val deriver = CoproductDeriver[ SuperT ]
        val sch = deriver.derive
        sch.subtypeDescriptions.size shouldBe 2
        sch.subtypeDescriptions.head.typeName shouldBe "SubT1"
    }

    it should "generate Subtype.fromSuper methods correctly" in {
        val shape = CoproductDeriver[ SuperT ].derive

        val maskedSt1Val : SuperT = SubT1( 3 )
        val st1 = shape.subtypeDescriptions.head
        val st2 = shape.subtypeDescriptions.tail.head
        st1.fromSuper( maskedSt1Val ) shouldBe Some( SubT1( 3 ) )
        st2.fromSuper( maskedSt1Val ) shouldBe None
    }

    sealed trait SupS
    case object SubS1 extends SupS
    case object SubS2 extends SupS

    it should "derive a coproduct consisting of case objects using singleton schemas" in {
        val shape = CoproductDeriver[ SupS ].derive

        val sts = shape.subtypeDescriptions
        sts.size shouldBe 2
        val st1 = sts.head
        val st2 = sts.tail.head

        st1.schema.shape shouldBe SingletonShape[ SubS1.type, "SubS1" ]( "SubS1", SubS1 )
        st2.schema.shape shouldBe SingletonShape[ SubS2.type, "SubS2" ]( "SubS2", SubS2 )
    }

    it should "derive a coproduct with a lazy subtype when the subtype is an ancestor" in  {
        val shape = CoproductDeriver.withAncestors[ SupS, SubS1.type *: Tuple ].derive

        val sts = shape.subtypeDescriptions
        sts.size shouldBe 2
        val st1 = sts.head
        val st2 = sts.tail.head

        st1 match {
            case LazySubtype( typeName, _, _, _, _, _, _, _, _, _ ) =>
            case _ => fail( "subs1 was not lazy" )
        }
        st2.schema.shape shouldBe SingletonShape[ SubS2.type, "SubS2" ]( "SubS2", SubS2 )
    }

}
