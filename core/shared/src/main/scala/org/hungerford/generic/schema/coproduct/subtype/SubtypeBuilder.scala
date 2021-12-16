package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.coproduct.ValidDiscriminator
import org.hungerford.generic.schema.coproduct.subtype.SubtypeBuilder.empty
import org.hungerford.generic.schema.product.field.FieldBuilderWithSchemaWithoutName
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.types.Sub

import scala.util.NotGiven

case class SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] (
    private[subtype] val tn: N,
    private[subtype] val sch: Sch,
    private[subtype] val as : AS,
    private[subtype] val dv : DV,
    private[subtype] val desc: Option[ String ] = None,
    private[subtype] val vals: Set[ Validator[ ST ] ] = Set.empty[ Validator[ ST ] ],
    private[subtype] val df: Option[ ST ] = None,
    private[subtype] val exs: Seq[ ST ] = Seq.empty[ ST ],
    private[subtype] val dep: Boolean = false,
) {
    def typeName[ NewN <: TypeName ]( name : NewN ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, NewN, S, Sch ] =
        copy[ T, ST, D, DN, DV, AS, NewN, S, Sch ]( tn = name )

    def fromSchema[ NewS ]( implicit schema : Schema.Aux[ ST, NewS ] ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, NewS, Schema.Aux[ ST, NewS ] ] =
        copy[ T, ST, D, DN, DV, AS, N, NewS, Schema.Aux[ ST, NewS ] ]( sch = schema )

    def primitive : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, Unit, Schema.Aux[ ST, Unit ] ] =
        fromSchema( Primitive[ ST ]() )

    def asSuper( fn : ST => T ) : SubtypeBuilder[ T, ST, D, DN, DV, ST => T, N, S, Sch ] =
        copy[ T, ST, D, DN, DV, ST => T, N, S, Sch ]( as = fn )

    def discriminatorValue[ NewDV <: D ]( value : NewDV ) : SubtypeBuilder[ T, ST, D, DN, NewDV, AS, N, S, Sch ] =
        copy[ T, ST, D, DN, NewDV, AS, N, S, Sch ]( dv = value )

    def description( description : String ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] = copy( desc = Some( description ) )
    def validate( validators : Validator[ ST ]* ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] = copy( vals = vals ++ validators.toSet )
    def examples( examples : ST* ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] = copy( exs = exs ++ examples )
    def default( defaultValue : ST ) : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] = copy( df = Some( defaultValue ) )
    def deprecate : SubtypeBuilder[ T, ST, D, DN, DV, AS, N, S, Sch ] = copy( dep = true )

    def build[ NCorrect <: TypeName ](
        using
        schEv : Sch =:= Schema.Aux[ ST, S ],
        asEv : AS =:= (ST => T),
        dvEv : DV <:< D,
        tnEv : Sub.Aux[ N, TypeName, NCorrect ],
        vd : ValidDiscriminator[ D, DN, Subtype.Aux[ T, ST, D, DN, DV, NCorrect, S ] *: EmptyTuple ],
    ) : Subtype.Aux[ T, ST, D, DN, DV, NCorrect, S ] =
        SubtypeCase[ T, ST, D, DN, DV, NCorrect, S ](
            tnEv( tn ),
            schEv( sch ),
            asEv( as ),
            dv,
            desc,
            vals,
            df,
            exs,
            dep,
        )
}

trait AsSuperGenerator[ T, ST ] {
    type AS

    def as : AS
}

object AsSuperGenerator {
    type Aux[ T, ST, ASType ] = AsSuperGenerator[ T, ST ] { type AS = ASType }

    given [ T, ST ]( using ev : ST <:< T ) : AsSuperGenerator[ T, ST ] with {
        type AS = ST => T

        def as : ST => T = ( st : ST ) => ev( st )
    }

    given notSubtype[ T, ST ]( using ev : NotGiven[ ST <:< T ] ) : AsSuperGenerator[ T, ST ] with {
        type AS = Unit

        def as : Unit = ()
    }
}

object SubtypeBuilder {
    def empty[ T, ST, D, DN ](
        using
        asEv : AsSuperGenerator[ T, ST ],
    ) : SubtypeBuilder[ T, ST, D, DN, Unit, asEv.AS, Unit, Nothing, Unit ] =
        SubtypeBuilder[ T, ST, D, DN, Unit, asEv.AS, Unit, Nothing, Unit ](
            (),
            (),
            asEv.as,
            (),
        )
}
