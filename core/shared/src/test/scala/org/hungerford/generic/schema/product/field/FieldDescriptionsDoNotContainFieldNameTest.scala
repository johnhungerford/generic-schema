package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FieldDescriptionsDoNotContainFieldNameTest extends AnyFlatSpecLike with Matchers {

    behavior of "FieldDescriptionsDoNotContainFieldNameTest"

    it should "provide an implicit instance for a 1-tuple without the fieldname" in {
        assertCompiles( """summon[ FieldDescriptionsDoNotContainFieldName[ "name", FieldDescription.Aux[ Int, "other_name", Unit ] *: EmptyTuple ] ]""" )
    }

    it should "fail to provide an implicit instance for a 1-tuple with the same fieldname" in {
        assertDoesNotCompile( """summon[ FieldDescriptionsDoNotContainFieldName[ "name", FieldDescription.Aux[ Int, "name", Unit ] *: EmptyTuple ] ]""" )
    }

}
