package org.hungerford.generic.schema.types

trait Nat
class _0 extends Nat
class Succ[ N <: Nat ] extends Nat

object Nat {
    type _1 = Succ[ _0 ]
    type _2 = Succ[ _1 ]
    type _3 = Succ[ _2 ]
    type _4 = Succ[ _3 ]
    type _5 = Succ[ _4 ]
    type _6 = Succ[ _5 ]
    type _7 = Succ[ _6 ]
    type _8 = Succ[ _7 ]
    type _9 = Succ[ _8 ]
    type _10 = Succ[ _9 ]
    type _11 = Succ[ _10 ]
    type _12 = Succ[ _11 ]
    type _13 = Succ[ _12 ]
    type _14 = Succ[ _13 ]
    type _15 = Succ[ _14 ]
    type _16 = Succ[ _15 ]
    type _17 = Succ[ _16 ]
    type _18 = Succ[ _17 ]
    type _19 = Succ[ _18 ]
    type _20 = Succ[ _19 ]
    type _21 = Succ[ _20 ]
    type _22 = Succ[ _21 ]

    type Dec[ A <: Nat ] = Minus[ A, _1 ]
    type DecA[ A1 <: Nat, A <: Nat ] = Minus.Aux[ A1, _1, A ]

    type Int[ I <: scala.Int ] = IntEquiv[ I ]
    type IntA[ I <: scala.Int, N <: Nat ] = IntEquiv.Aux[ I, N ]
}

trait Plus[ A <: Nat, B <: Nat ] {
    type Res <: Nat
}

object Plus {
    type Aux[ A <: Nat, B <: Nat, C <: Nat ] = Plus[ A, B ] { type Res = C }

    given [ B <: Nat ] : Plus[ _0, B ] with { type Res = B }
    given leftRecurse[ A <: Nat, B <: Nat, C <: Nat ](
        using
        ev : Plus.Aux[ A, B, C ],
    ) : Plus[ Succ[ A ], B ] with { type Res = Succ[ C ] }
    given rightRecurse[ A <: Nat, B <: Nat, C <: Nat ](
        using
        ev : Plus.Aux[ A, B, C ],
    ) : Plus[ A, Succ[ B ] ] with { type Res = Succ[ C ] }
    given bothRecurse[ A <: Nat, B <: Nat, C <: Nat ](
        using
        ev : Plus.Aux[ A, B, C ],
    ) : Plus[ Succ[ A ], Succ[ B ] ] with { type Res = Succ[ Succ[ C ] ] }
}

trait Minus[ A <: Nat, B <: Nat ] {
    type Res <: Nat
}

object Minus {
    type Aux[ A <: Nat, B <: Nat, C <: Nat ] = Minus[ A, B ] { type Res = C }

    given [ A <: Nat, B <: Nat, C <: Nat ](
        using
        ev : Plus.Aux[ A, B, C ],
    ) : Minus[ C, B ] with { type Res = A }
}

trait IntEquiv[ I <: Int ] {
    type N <: Nat
}

object IntEquiv {
    type Aux[ I <: Int, N0 <: Nat ] = IntEquiv[ I ] { type N = N0 }

    given eq0 : IntEquiv[ 0 ] with { type N = _0 }
    given eq1 : IntEquiv[ 1 ] with { type N = Nat._1 }
    given eq2 : IntEquiv[ 2 ] with { type N = Nat._2 }
    given eq3 : IntEquiv[ 3 ] with { type N = Nat._3 }
    given eq4 : IntEquiv[ 4 ] with { type N = Nat._4 }
    given eq5 : IntEquiv[ 5 ] with { type N = Nat._5 }
    given eq6 : IntEquiv[ 6 ] with { type N = Nat._6 }
    given eq7 : IntEquiv[ 7 ] with { type N = Nat._7 }
    given eq8 : IntEquiv[ 8 ] with { type N = Nat._8 }
    given eq9 : IntEquiv[ 9 ] with { type N = Nat._9 }
    given eq10 : IntEquiv[ 10 ] with { type N = Nat._10 }
    given eq11 : IntEquiv[ 11 ] with { type N = Nat._11 }
    given eq12 : IntEquiv[ 11 ] with { type N = Nat._12 }
    given eq13 : IntEquiv[ 11 ] with { type N = Nat._13 }
    given eq14 : IntEquiv[ 11 ] with { type N = Nat._14 }
    given eq15 : IntEquiv[ 11 ] with { type N = Nat._15 }
    given eq16 : IntEquiv[ 11 ] with { type N = Nat._16 }
    given eq17 : IntEquiv[ 11 ] with { type N = Nat._17 }
    given eq18 : IntEquiv[ 11 ] with { type N = Nat._18 }
    given eq19 : IntEquiv[ 11 ] with { type N = Nat._19 }
    given eq20 : IntEquiv[ 11 ] with { type N = Nat._20 }
    given eq21 : IntEquiv[ 11 ] with { type N = Nat._21 }
    given eq22 : IntEquiv[ 11 ] with { type N = Nat._22 }


}
