package org.hungerford.generic.schema.product

import org.scalatest.flatspec.AnyFlatSpecLike
import shapeless._

class CtxWrapHListsConstraintTest extends AnyFlatSpecLike {

    behavior of "CtxWrapHlistsConstraint"

    it should "compile when comparing HNil with HNil, regardless of context" in {
        assertCompiles( "CtxWrapHListsConstraint[ List, HNil, HNil ]" )
        assertCompiles( "CtxWrapHListsConstraint[ Vector, HNil, HNil ]" )
        assertCompiles( "import scala.concurrent.Future; CtxWrapHListsConstraint[ Future, HNil, HNil ]" )
    }

    it should "compile when comparing an HList of vectors of values and an HList of values with corresponding types" in {
        type Rt = List[ Int ] :: List[ Double ] :: List[ String ] :: HNil
        type RVt = Int :: Double :: String :: HNil

        assertCompiles( "CtxWrapHListsConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list is longer" in {
        type Rt = List[ Int ] :: HNil
        type RVt = Int :: Double :: HNil

        assertDoesNotCompile( "CtxWrapHListsConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list is shorter" in {
        type Rt = List[ Int ] :: List[ Double ] :: HNil
        type RVt = Int :: HNil

        assertDoesNotCompile( "CtxWrapHListsConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when value list has different types from context types" in {
        type Rt = List[ Int ] :: List[ Int ] :: List[ String ] :: HNil
        type RVt = Int :: Double :: String :: HNil

        assertDoesNotCompile( "CtxWrapHListsConstraint[ List, Rt, RVt ]" )
    }

    it should "not compile when context list has wrong context for one or more elements" in {
        type Rt = List[ Int ] :: List[ Int ] :: Vector[ String ] :: List[ Double ] :: HNil
        type RVt = Int :: Int :: String :: Double :: HNil

        assertDoesNotCompile( "CtxWrapHListsConstraint[ List, Rt, RVt ]" )
    }

}
