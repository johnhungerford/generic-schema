package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.coproduct.ValidDiscriminator
import org.hungerford.generic.schema.coproduct.subtype.SubtypeBuilder.empty
import org.hungerford.generic.schema.product.field.FieldBuilderWithSchemaWithoutName
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.types.Sub

import scala.util.NotGiven

case class SubtypeBuilder[ T, ST, D, DN, DV, TS, FS, N, S, Sch ] (
    private[subtype] val tn: N,
    private[subtype] val sch: Sch,
    private[subtype] val ts : TS,
    private[subtype] val fs : FS,
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

    def toSuper( fn : ST => T ) : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, FS, N, S, Sch ] =
        copy[ T, ST, D, DN, DV, ST => T, FS, N, S, Sch ]( ts = fn )

    def fromSuper( fn : T => Option[ ST ] ) : SubtypeBuilder[ T, ST, D, DN, DV, TS, T => Option[ ST ], N, S, Sch ] =
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
        vd : ValidDiscriminator[ D, DN, Subtype.Aux[ T, ST, D, DN, DV, CorrectN, S ] *: EmptyTuple ],
    ) : Subtype.Aux[ T, ST, D, DN, DV, CorrectN, S ] = {
        SubtypeCase[ T, ST, D, DN, DV, CorrectN, S ](
            tnEv( tn ),
            schEv( sch ),
            tsEv( ts ),
            fsEv( fs ),
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

trait FromSuperGenerator[ T, ST ] {
    type FS

    def fromSuper : FS
}

object FromSuperGenerator {
    type Aux[ T, ST, FSType ] = FromSuperGenerator[ T, ST ] { type FS = FSType }

    given [ T, ST ]( using ev : ST <:< T ) : FromSuperGenerator[ T, ST ] with {
        type FS = T => Option[ ST ]

        def fromSuper : T => Option[ ST ] = ( t : T ) => t match {
            case st : ST => Some( st )
            case _ => None
        }
    }

    given notSubtype[ T, ST ]( using ev : NotGiven[ ST <:< T ] ) : FromSuperGenerator[ T, ST ] with {
        type FS = Unit

        def fromSuper : Unit = ()
    }
}

object SubtypeBuilder {
    def empty[ T, ST, D, DN ](
        using
        tsEv : ToSuperGenerator[ T, ST ],
        fsEv : FromSuperGenerator[ T, ST ],
    ) : SubtypeBuilder[ T, ST, D, DN, Unit, tsEv.TS, fsEv.FS, Unit, Nothing, Unit ] =
        SubtypeBuilder[ T, ST, D, DN, Unit, tsEv.TS, fsEv.FS, Unit, Nothing, Unit ](
            (),
            (),
            tsEv.toSuper,
            fsEv.fromSuper,
            (),
        )

    def from[ T, ST, D, DN, DV, N <: TypeName, S ](
        subtype : Subtype.Aux[ T, ST, D, DN, DV, N, S ],
    ) : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, T => Option[ ST ], N, S, Schema.Aux[ ST, S ] ] = {
        SubtypeBuilder[ T, ST, D, DN, DV, ST => T, T => Option[ ST ], N, S, Schema.Aux[ ST, S ] ](
            subtype.typeName,
            subtype.schema,
            subtype.toSuper,
            subtype.fromSuper,
            subtype.discriminatorValue,
            subtype.description,
            subtype.validators,
            subtype.default,
            subtype.examples,
            subtype.deprecated,
        )
    }
}
