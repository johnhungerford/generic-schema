package org.hungerford.generic.schema.product

import org.scalatest.flatspec.AnyFlatSpecLike

class CtxWrapTuplesConstraintTest extends AnyFlatSpecLike {

    behavior of "CtxWrapHlistsConstraint"

    it should "compile when comparing EmptyTuple with EmptyTuple, regardless of context" in {
        assertCompiles( "CtxWrapTuplesConstraint[ List, EmptyTuple, EmptyTuple ]" )
        assertCompiles( "CtxWrapTuplesConstraint[ Vector, EmptyTuple, EmptyTuple ]" )
        assertCompiles( "import scala.concurrent.Future; CtxWrapTuplesConstraint[ Future, EmptyTuple, EmptyTuple ]" )
    }

    it should "compile when comparing an Tuple of vectors of values and an Tuple of values with corresponding types" in {
        type Rt = List[ Int ] *: List[ Double ] *: List[ String ] *: EmptyTuple
        type RVt = Int *: Double *: String *: EmptyTuple

        assertCompiles( "CtxWrapTuplesConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list is longer" in {
        type Rt = List[ Int ] *: EmptyTuple
        type RVt = Int *: Double *: EmptyTuple

        assertDoesNotCompile( "CtxWrapTuplesConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list is shorter" in {
        type Rt = List[ Int ] *: List[ Double ] *: EmptyTuple
        type RVt = Int *: EmptyTuple

        assertDoesNotCompile( "CtxWrapTuplesConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when value list has different types from context types" in {
        type Rt = List[ Int ] *: List[ Int ] *: List[ String ] *: EmptyTuple
        type RVt = Int *: Double *: String *: EmptyTuple

        assertDoesNotCompile( "CtxWrapTuplesConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list has wrong context for one or more elements" in {
        type Rt = List[ Int ] *: List[ Int ] *: Vector[ String ] *: List[ Double ] *: EmptyTuple
        type RVt = Int *: Int *: String *: Double *: EmptyTuple

        assertDoesNotCompile( "CtxWrapTuplesConstraint[ List, Rt, RVt ]" )
    }

}
