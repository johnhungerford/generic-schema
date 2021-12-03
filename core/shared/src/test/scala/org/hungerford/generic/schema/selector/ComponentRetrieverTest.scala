package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.SchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.FieldDescription
import org.scalatest.flatspec.AnyFlatSpecLike

class ComponentRetrieverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ComponentRetriever"

    case class Innerest( bool : Boolean, dbl : Double )
    case class Inner( innerest : Innerest, str : String )
    case class Outer( int : Int, inner : Inner )

    val sch = SchemaBuilder[ Outer ].caseClass.build

    it should "retrieve a nested field" in {
        val innerSch = SchemaBuilder[ Inner ].caseClass.build
        val fld = ComponentRetriever.retrieve( sch )( Selector.field( "inner" ) / "innerest" / "dbl" )
        summon[ fld.type <:< FieldDescription[ Double ] ]
        fld.fieldName shouldBe "dbl"
        fld.description shouldBe None
        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
        fld.schema.shape shouldBe ()
    }

}
