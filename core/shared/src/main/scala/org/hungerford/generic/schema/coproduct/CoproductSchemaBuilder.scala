package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.{FieldName, FieldRetriever}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}

import scala.collection.immutable.ListMap

case class CoproductSchemaBuilder[ T, R <: Tuple, D, DN ](
    private[ schema ] val nm : Option[ String ] = None,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ schema ] val exs : Seq[ T ] = Nil,
    private[ schema ] val dep : Boolean = false,
    private[ schema ] val sts : R,
) {
    def name( name : String ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( nm = Some( name ) )
    def description( description : String ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        copy( vals = validators.toSet )
    def examples( example : T, otherExamples : T* ) : CoproductSchemaBuilder[ T, R, D, DN ] =
        examples( example +: otherExamples )
    def examples( examples : Seq[ T ] ) : CoproductSchemaBuilder[ T, R, D, DN ] = copy( exs = examples.toSeq )
    def deprecate : CoproductSchemaBuilder[ T, R, D, DN ] = copy( dep = true )

    def discriminator[ NewD ](
        using
        da : DiscriminatorAdder[ NewD, T, R, D, DN ],
    ) : [ NewDN <: FieldName ] => NewDN => da.Out[ NewDN ] = da.add( nm, desc, vals, exs, dep, sts )

    def addSubtype[ ST, DV, N <: TypeName, S ](
        st : Subtype.Aux[ T, ST, D, DN, DV, N, S ],
    ) : CoproductSchemaBuilder[ T, Tuple.Concat[ R, Subtype.Aux[ T, ST, D, DN, DV, N, S ] *: EmptyTuple ], D, DN ] = {
        copy[ T, Tuple.Concat[ R, Subtype.Aux[ T, ST, D, DN, DV, N, S ] *: EmptyTuple ], D, DN ](
            sts = sts ++ ( st *: EmptyTuple ),
        )
    }

}

object CoproductSchemaBuilder {
    def empty[ T ] = new CoproductSchemaBuilder[ T, EmptyTuple, Unit, Nothing ](
        sts = EmptyTuple,
    )
}

trait DiscriminatorAdder[ NewD, T, R, D, DN ] {
    type Out[ _ ]

    def add(
        nm : Option[ String ] = None,
        desc : Option[ String ] = None,
        vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
        exs : Seq[ T ] = Nil,
        dep : Boolean = false,
        sts : R,
    ) : [ NewDN <: FieldName ] => NewDN => Out[ NewDN ]

}

object DiscriminatorAdder {
    given [ T, R <: Tuple, D, DN ] : DiscriminatorAdder[ D, T, R, D, DN ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, D, NewDN ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, D, NewDN ](
                nm, desc, vals, exs, dep, sts,
            )
        }
    }

    given [ NewD, T, R <: Tuple, D, DN ] : DiscriminatorAdder[ NewD, T, R, D, DN ] with {
        type Out[ NewDN ] = CoproductSchemaBuilder[ T, R, NewD, NewDN ]

        override def add(
            nm: Option[ String ],
            desc: Option[ String ],
            vals: Set[ Validator[ T ] ],
            exs: Seq[ T ],
            dep: Boolean,
            sts: R,
        ): [ NewDN <: FieldName ] => NewDN => Out[ NewDN ] = { [ NewDN <: FieldName ] => ( _ : NewDN ) =>
            CoproductSchemaBuilder[ T, R, NewD, NewDN ](
                nm, desc, vals, exs, dep, sts,
            )
        }
    }
}

trait SubtypeAdder[ ST, T, R <: Tuple, D, DN ] {
    type Evidence[ _ <: TypeName, _ ]
    type AddParameters[ _ <: TypeName, _ ]
    type AddResult[ _ <: TypeName, _ ]
    type AdderType =
        ( Option[ String ], Option[ String ], Set[ Validator[ T ] ], Seq[ T ], Boolean, R ) => AddFn
    type AddFn = [ N <: FieldName, STS ]=>
        Evidence[ N, STS ] ?=>
            AddParameters[ N, STS ] =>
              AddResult[ N, STS ]

    def add : AdderType
}
