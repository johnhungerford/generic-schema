package org.hungerford.generic.schema

object TestADTs {

    object Coproduct {

        sealed trait SealedTrait
        final case class SealedTraitSub1( int : Int ) extends SealedTrait
        final case class SealedTraitSub2( str : String ) extends SealedTrait
        final case class SealedTraitSub3( bool : Boolean )

        sealed abstract class SealedAbstractClass
        final case class SealedAbstractClassSub1( int : Int ) extends SealedTrait
        final case class SealedAbstractClassSub2( str : String ) extends SealedTrait
        final case class SealedAbstractClassSub3( bool : Boolean )

        sealed trait SealedTraitWithObject
        object SealedTraitWithObject {
            final case class SealedTraitWithObjectSub1( int : Int ) extends SealedTraitWithObject
            final case class SealedTraitWithObjectSub2( str : String ) extends SealedTraitWithObject
            final case class SealedTraitWithObjectSub3( bool : Boolean ) extends SealedTraitWithObject
        }

        sealed abstract class SealedAbstractClassWithObject
        object SealedAbstractClassWithObject:
            final case class SealedAbstractClassSub1( int : Int ) extends SealedTrait
            final case class SealedAbstractClassSub2( str : String ) extends SealedTrait
            final case class SealedAbstractClassSub3( bool : Boolean )

        enum EnumCoproduct:
            case EnumCoproductSub1( int : Int )
            case EnumCoproductSub2( str : String )
            case EnumCoproductSub3( bool : Boolean )

    }

    object Complex {

        case class ComplexProd( a : Any, b : Any )
        sealed trait ComplexProdA
        final case object ComplexProdA1 extends ComplexProdA
        final case class ComplexProdA2( c : Int, d : String, e : Boolean ) extends ComplexProdA
        final case class ComplexProdA3( f : ComplexProdA3F, g : Long ) extends ComplexProdA
        case class ComplexProdA3F( h : Double, i : Int, j : ComplexProdA3FK )
        case class ComplexProdB( k : ComplexProdA3F, l : ComplexProdA3FK )
        enum ComplexProdA3FK:
            case ComplexProdA3FK1( m : Boolean )
            case ComplexProdA3FK2( n : Double )
            case ComplexProdA3FK3( o : String )
            case ComplexProdA3FK4

    }

    object Recursive {

        final case class RecursiveProduct( a : Int, b : Boolean, c : RecursiveCoproduct ) extends RecursiveCoproduct
        sealed trait RecursiveCoproduct
        final case object Terminal extends RecursiveCoproduct

        case class Outer( field1 : Int, field2 : Inner1 )
        case class Inner1( field1 : Inner2, field2 : String )
        case class Inner2( field1 : Boolean, field2 : Inner1, field3 : Inner3 )
        sealed trait Inner3
        case object Inner3Sub1 extends Inner3
        case class Inner3Sub2( field1 : Double, field2 : Int, field3 : Inner4 ) extends Inner3
        case class Inner3Sub3( field1 : Inner2 ) extends Inner3
        case class Inner3Sub4( field1 : Inner1, field2 : Inner2, field3 : Inner4 )
        case class Inner4( field1 : Inner3 )

    }

}
