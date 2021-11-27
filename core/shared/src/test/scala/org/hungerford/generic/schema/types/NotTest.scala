package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class NotTest extends AnyFlatSpecLike with Matchers {
    behavior of "Not"

    it should "be summonable for a type without given instances" in {
        assertDoesNotCompile( """summon[ Int =:= String ]""" )
        assertCompiles( """summon[ Not[ Int =:= String ] ]""" )
        assertDoesNotCompile( """summon[ List[ Int ] ]""" )
        assertCompiles( """summon[ Not[ List[ Int ] ] ]""" )
        assertDoesNotCompile( """summon[ Any <:< Nothing ]""" )
        assertCompiles( """summon[ Not[ Any <:< Nothing ] ]""" )
    }

    it should "not be summonable for a type with a given instance" in {
        assertCompiles( """summon[ Int =:= Int ]""" )
        assertDoesNotCompile( """summon[ Not[ Int =:= Int ] ]""" )
        assertCompiles( """summon[ Nothing <:< Any ]""")
        assertDoesNotCompile( """summon[ Not[ Nothing <:< Any ] ]""")
    }

}
