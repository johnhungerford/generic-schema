package org.hungerford.generic.schema.selector

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.Field

class ComponentRetrieverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ComponentRetriever"

    case class Innerest( bool : Boolean, dbl : Double )
    case class Inner( innerest : Innerest, str : String )
    case class Outer( int : Int, inner : Inner )

    val sch = Schema.derived[ Outer ]

    it should "retrieve a nested field" in {
        val innerSch = Schema.derived[ Inner ]
        val fld = ComponentRetriever.retrieve( sch )( Selector.field( "inner" ) / "innerest" / "dbl" )
        summon[ fld.type <:< Field[ Double ] ]
        fld.fieldName shouldBe "dbl"
        fld.description shouldBe None
        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
        fld.schema.shape shouldBe ()
    }

}
