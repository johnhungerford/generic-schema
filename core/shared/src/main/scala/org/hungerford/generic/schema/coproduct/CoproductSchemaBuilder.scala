package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.{FieldName, FieldRetriever}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}

import scala.collection.immutable.ListMap

case class CoproductSchemaBuilder[ T, R <: Tuple, D, DN, DV <: Tuple ](
    private[ schema ] val nm : Option[ String ] = None,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ schema ] val exs : Seq[ T ] = Nil,
    private[ schema ] val dep : Boolean = false,
    private[ schema ] val sts : R,
    private[ schema ] val dvs : DV,
) {
    def name( name : String ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] = copy( nm = Some( name ) )
    def description( description : String ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] =
        copy( vals = validators.toSet )
    def examples( example : T, otherExamples : T* ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] =
        examples( example +: otherExamples )
    def examples( examples : Seq[ T ] ) : CoproductSchemaBuilder[ T, R, D, DN, DV ] = copy( exs = examples.toSeq )
    def deprecate : CoproductSchemaBuilder[ T, R, D, DN, DV ] = copy( dep = true )

    def discriminator[ NewD ](
        using
        da : DiscriminatorAdder[ NewD, T, R, D, DN, DV ],
    ) : [ NewDN <: FieldName ] => NewDN => da.Out[ NewDN ] = da.add( nm, desc, vals, exs, dep, sts, dvs )

    def addSubtype[ ST ](
        using
        sa : SubtypeAdder[ ST, T, R, D, DN, DV ],
    ) : sa.AddFn = sa.add( nm, desc, vals, exs, dep, sts, dvs )

}

object CoproductSchemaBuilder {
    def empty[ T ] = new CoproductSchemaBuilder[ T, EmptyTuple, Nothing, Nothing, EmptyTuple ](
        sts = EmptyTuple,
        dvs = EmptyTuple,
    )
}

trait DiscriminatorAdder[ NewD, T, R, D, DN, DV ] {
    type Out[ _ ]

    def add(
        nm : Option[ String ] = None,
        desc : Option[ String ] = None,
        vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
        exs : Seq[ T ] = Nil,
        dep : Boolean = false,
        sts : R,
        dvs : DV,
    ) : [ NewDN <: FieldName ] => NewDN => Out[ NewDN ]

}

object DiscriminatorAdder {
    given [ T, R <: Tuple, D, DN, DV <: Tuple ] : DiscriminatorAdder[ D, T, R, D, DN, DV ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, D, NewDN, DV ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
            dvs: DV,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, D, NewDN, DV ](
                nm, desc, vals, exs, dep, sts, dvs,
            )
        }
    }

    given [ NewD, T, R <: Tuple, D, DN, DV <: Tuple ] : DiscriminatorAdder[ NewD, T, R, D, DN, DV ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, NewD, NewDN, EmptyTuple ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
            dvs: DV,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, NewD, NewDN, EmptyTuple ](
                nm, desc, vals, exs, dep, sts, EmptyTuple,
            )
        }
    }
}

trait SubtypeAdder[ ST, T, R <: Tuple, D, DN, DV <: Tuple ] {
    type Evidence[ _ <: TypeName, _ ]
    type AddParameters[ _ <: TypeName, _ ]
    type AddResult[ _ <: TypeName, _ ]
    type AdderType =
        ( Option[ String ], Option[ String ], Set[ Validator[ T ] ], Seq[ T ], Boolean, R, DV ) => AddFn
    type AddFn = [ N <: FieldName, STS ]=>
        Evidence[ N, STS ] ?=>
            AddParameters[ N, STS ] =>
              AddResult[ N, STS ]

    def add : AdderType
}

object SubtypeAdder {
    given noDiscriminator[ ST, T, R <: Tuple ] : SubtypeAdder[ ST, T, R, Nothing, Nothing, EmptyTuple ] with {
        type Evidence[ _ <: TypeName, _ ] = R <:< Tuple // trivial
        type AddParameters[ XN <: TypeName, XS ] = Subtype.Aux[ ST, XN, XS ]
        type AddResult[ XN <: TypeName, XS ] =
            CoproductSchemaBuilder[ T, Tuple.Concat[ R, AddParameters[ XN, XS ] *: EmptyTuple ], Nothing, Nothing, EmptyTuple ]

        def add : AdderType =
            (
                nm: Option[ String ],
                desc: Option[ String ],
                vals: Set[ Validator[ T ] ],
                exs: Seq[ T ],
                dep: Boolean,
                sts: R,
                dvs: EmptyTuple
            ) => [ N <: FieldName, STS ] => ( ev : R <:< Tuple ) ?=> ( params: AddParameters[ N, STS ] ) => {
                val newSts: Tuple.Concat[ R, AddParameters[ N, STS ] *: EmptyTuple ] = sts ++ ( params
                  *: EmptyTuple )
                CoproductSchemaBuilder[ T, Tuple.Concat[ R, AddParameters[ N, STS ] *: EmptyTuple ], Nothing,
                  Nothing, EmptyTuple ](
                    nm, desc, vals, exs, dep, newSts, EmptyTuple,
                )
            }
    }

    given withDiscriminator[ ST, T, R <: Tuple, D, DN, DV <: Tuple ] : SubtypeAdder[ ST, T, R, D, DN, DV ] with {
        type Evidence[ XN <: TypeName, XS ] = ValidDiscriminator[ D, DN, Tuple.Concat[ R, Subtype.Aux[ ST, XN, XS ] *: EmptyTuple ] ]
        type AddParameters[ XN <: TypeName, XS ] = (Subtype.Aux[ ST, XN, XS ], D)
        type AddResult[ XN <: TypeName, XS ] =
            CoproductSchemaBuilder[ T, Tuple.Concat[ R, Subtype.Aux[ ST, XN, XS ] *: EmptyTuple ], D, DN, Tuple.Concat[ DV, D *: EmptyTuple ] ]

        def add : AdderType =
            ( nm: Option[ String ],
              desc: Option[ String ],
              vals: Set[ Validator[ T ] ],
              exs: Seq[ T ],
              dep: Boolean,
              sts: R,
              dvs: DV ) => [ N <: FieldName, STS ] =>
                  ( ev : ValidDiscriminator[ D, DN, Tuple.Concat[ R, Subtype.Aux[ ST, N, STS ] *: EmptyTuple ] ] ) ?=>
                    ( params: AddParameters[ N, STS ] ) => {
                        val newSts: Tuple.Concat[ R, Subtype.Aux[ ST, N, STS ] *: EmptyTuple ] = sts ++ ( params._1
                          *: EmptyTuple )
                        val newDvs: Tuple.Concat[ DV, D *: EmptyTuple ] = dvs ++ ( params._2 *: EmptyTuple )
                        CoproductSchemaBuilder[ T, Tuple.Concat[ R, Subtype.Aux[ ST, N, STS ] *: EmptyTuple ], D, DN,
                          Tuple.Concat[ DV, D *: EmptyTuple ] ](
                            nm, desc, vals, exs, dep, newSts, newDvs,
                        )
            }
    }
}
