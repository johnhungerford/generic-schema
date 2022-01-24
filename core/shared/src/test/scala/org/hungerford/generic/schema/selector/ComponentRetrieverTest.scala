package org.hungerford.generic.schema.selector

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.coproduct.subtype.Subtype

class ComponentRetrieverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ComponentRetriever"

    case class Innerest( bool : Boolean, dbl : Double )
    case class Inner( innerest : Innerest, str : String )
    case class Outer( int : Int, inner : Inner )

    val sch = Schema.derived[ Outer ]

    it should "retrieve a nested field" in {
        val innerSch = Schema.derived[ Inner ]
        val fld = ComponentRetriever.retrieve( sch )( Selector.field( "inner" ) /- "innerest" /- "dbl" )
        summon[ fld.type <:< Field[ Innerest, Double ] ]
        fld.fieldName shouldBe "dbl"
        fld.description shouldBe None
        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
        fld.schema.shape shouldBe ()
    }

    sealed trait OuterT
    final case class SubT() extends OuterT
    sealed trait InnerT extends OuterT
    final case class SubT1() extends InnerT
    final case class SubT2(int: Int) extends InnerT
    sealed trait CoreT extends InnerT
    final case class SubT3(str : String) extends CoreT

    it should "retrieve a nested subtype" in {
        val sch = Schema.derived[ OuterT ]

        val st = ComponentRetriever.retrieve( sch )( Selector.subtype( "InnerT" ) /~ "CoreT" /~ "SubT3" )
        summon[ st.type <:< Subtype[ CoreT, SubT3, Unit ] ]
        st.typeName shouldBe "SubT3"
        st.description shouldBe None
        st.validators shouldBe Set.empty[ Validator[ SubT3 ] ]
        st.schema.shape.fieldDescriptions.head.fieldName shouldBe "str"
    }

    it should "retrieve a subtype from a schema builder" in {
        val schBuilder = Schema.derivedBuilder[ OuterT ]

        val st = ComponentRetriever.retrieve( schBuilder )( Selector.subtype( "InnerT" ) )
        st.typeName shouldBe "InnerT"
    }

}
