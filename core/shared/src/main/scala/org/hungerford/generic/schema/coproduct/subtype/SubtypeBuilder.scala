package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.coproduct.ValidDiscriminator
import org.hungerford.generic.schema.coproduct.subtype.SubtypeBuilder.empty
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.{ComplexSchema, NoSchema, Primitive, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.types.Sub

import scala.util.NotGiven

case class SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] (
    private[subtype] val tn: N,
    private[subtype] val sch: Sch,
    private[subtype] val ts : TS,
    private[subtype] val fs : FS,
    private[subtype] val dn : DN,
    private[subtype] val dv : DV,
    private[subtype] val desc: Option[ String ] = None,
    private[subtype] val vals: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ],
    private[subtype] val df: Option[ ST ] = None,
    private[subtype] val exs: Seq[ ST ] = Seq.empty[ ST ],
    private[subtype] val dep: Boolean = false,
) {
    def typeName[ NewN <: TypeName ]( name : NewN ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, NewN, S, Sch ] =
        copy[ T, ST, D, DN, DV, TS, FS, NewN, S, Sch ]( tn = name )

    def fromSchema[ NewS ]( implicit schema : Schema.Aux[ ST, NewS ] ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, NewS, Schema.Aux[ ST, NewS ] ] =
        copy[ T, ST, D, DN, DV, TS, FS, N, NewS, Schema.Aux[ ST, NewS ] ]( sch = schema )

    def primitive : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, Unit, Schema.Aux[ ST, Unit ] ] =
        fromSchema( Primitive[ ST ]() )

    def singleton[ TN <: TypeName, SV <: Singleton & ST ](
        using
        ev1 : Sub.Aux[ST, Singleton, SV],
        ev2 : Sub.Aux[N, TypeName, TN],
        vo : ValueOf[ SV ],
    ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, SingletonShape[SV, TN], Schema.Aux[ ST, SingletonShape[SV, TN] ] ] =
        fromSchema( ComplexSchema( SingletonShape[ SV, TN ]( ev2(tn), vo.value ) ) )

    def toSuper( fn : ST => T ) : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, FS, N, S, Sch ] =
        copy[ T, ST, D, DN, DV, ST => T, FS, N, S, Sch ]( ts = fn )

    def fromSuper( fn : T => Option[ ST ] ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, T => Option[ ST ], N, S, Sch ] =
        copy[ T, ST, D, DN, DV, TS, T => Option[ ST ], N, S, Sch ]( fs = { ( value : T ) =>
            val stValOpt: Option[ ST ] = fn( value )
            stValOpt flatMap { stVal =>
                if ( vals.exists( validator => !validator.isValid( stVal ) ) ) None
                else stValOpt
            }
        } )

    def fromSuperWithoutValidation( fn : T => Option[ ST ] ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, T => Option[ ST ], N, S, Sch ] =
        copy[ T, ST, D, DN, DV, TS, T => Option[ ST ], N, S, Sch ]( fs = fn )

    def discriminatorValue[ NewDV <: (D & Singleton) ]( value : NewDV ) : SubtypeBuilder[ T, ST, D, DN, NewDV, TS, FS, N, S, Sch ] =
        copy[ T, ST, D, DN, NewDV, TS, FS, N, S, Sch ]( dv = value )

    def description( description : String ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] = copy( desc = Some( description ) )
    def validate( validators : Validator[ ST ]* ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] = copy( vals = vals ++ validators.toSet )
    def examples( examples : ST* ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] = copy( exs = exs ++ examples )
    def default( defaultValue : ST ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] = copy( df = Some( defaultValue ) )
    def deprecate : SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] = copy( dep = true )

    def build[ CorrectN <: TypeName ](
        using
        schEv : Sch =:= Schema.Aux[ ST, S ],
        tsEv : TS =:= ( ST => T ),
        fsEv : FS =:= ( T => Option[ ST ] ),
        dvEv : CorrectDV[ D, DV ],
        tnEv : Sub.Aux[ N, TypeName, CorrectN ],
        vd : ValidDiscriminator[ D, DN, Subtype[ T, ST, D, DN, DV, CorrectN, S ] *: EmptyTuple ],
    ) : Subtype[ T, ST, D, DN, DV, CorrectN, S ] = {
        Subtype[ T, ST, D, DN, DV, CorrectN, S ](
            tnEv( tn ),
            schEv( sch ),
            tsEv( ts ),
            fsEv( fs ),
            dn,
            dv,
            desc,
            vals,
            df,
            exs,
            dep,
        )
    }
}

trait CorrectDV[ D, DV ] {
    type Out

    def apply( dv: DV ) : Out
}

object CorrectDV {
    type Aux[ D, DV, O ] = CorrectDV[ D, DV ] { type Out = O }
    given [ D, DV ]( using ev: Sub[ DV, D & Singleton ] ) : CorrectDV[ D, DV ] with {
        type Out = ev.ASub
        override def apply( dv: DV ): ev.ASub = ev( dv )
    }
    given CorrectDV[ Unit, Unit ] with {
        type Out = Unit
        override def apply( dv: Unit ): Unit = dv
    }
}

trait ToSuperGenerator[ T, ST ] {
    type TS

    def toSuper : TS
}

object ToSuperGenerator {
    type Aux[ T, ST, TSType ] = ToSuperGenerator[ T, ST ] { type TS = TSType }

    given [ T, ST ]( using ev : ST <:< T ) : ToSuperGenerator[ T, ST ] with {
        type TS = ST => T

        def toSuper : ST => T = ( v : ST ) => ev( v )
    }

    given notSubtype[ T, ST ]( using ev : NotGiven[ ST <:< T ] ) : ToSuperGenerator[ T, ST ] with {
        type TS = Unit

        def toSuper : Unit = ()
    }
}

object SubtypeBuilder {
    def empty[ T, ST, D, DN ](
        discriminatorName : DN,
    )(
        using
        tsEv : ToSuperGenerator[ T, ST ],
    ) : SubtypeBuilder[ T, ST, D, DN, Unit, tsEv.TS, Unit, Unit, Nothing, Unit ] =
        SubtypeBuilder[ T, ST, D, DN, Unit, tsEv.TS, Unit, Unit, Nothing, Unit ](
            (),
            (),
            tsEv.toSuper,
            (),
            discriminatorName,
            (),
        )

    def from[ T, ST, D, DN, DV, N <: TypeName, S ](
        subtype : Subtype[ T, ST, D, DN, DV, N, S ],
    ) : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, T => Option[ ST ], N, S, Schema.Aux[ ST, S ] ] = {
        SubtypeBuilder[ T, ST, D, DN, DV, ST => T, T => Option[ ST ], N, S, Schema.Aux[ ST, S ] ](
            subtype.typeName,
            subtype.schema,
            subtype.toSuper,
            subtype.fromSuper,
            subtype.discriminatorName,
            subtype.discriminatorValue,
            subtype.description,
            subtype.validators,
            subtype.default,
            subtype.examples,
            subtype.deprecated,
        )
    }
}
