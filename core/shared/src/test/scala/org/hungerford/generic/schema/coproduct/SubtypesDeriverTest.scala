package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.LazySubtype

import org.scalatest.flatspec.AnyFlatSpecLike

class SubtypesDeriverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SubtypesDeriver"

    sealed trait Super
    case class Sub() extends Super

    it should "derive a lazy subtype" in {
        val deriver = summon[ SubtypesDeriver[ Super, Sub *: EmptyTuple, Sub *: EmptyTuple, "Sub" *: EmptyTuple ] ]
        val sts = deriver.derive( 0 )
        sts.head match {
            case LazySubtype( typeName, _, _, _, _, _, _, _, _, _ ) =>
            case _ => fail( "subtype was not lazy" )
        }
    }

}
