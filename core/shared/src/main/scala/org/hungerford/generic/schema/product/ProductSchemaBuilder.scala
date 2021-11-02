package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.{PrimitiveSchemaBuilder, Schema, SchemaBuilder}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, FieldDescriptionBuilderWithoutSchema}
import org.hungerford.generic.schema.validator.Validator
import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless._

case class ProductSchemaBuilder[ T, R <: HList, RV <: HList, AF, AFS <: Schema[ AF ] ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : AFS,
    private[ product ] val fieldDescs : R,
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler[ RV ],
) {
    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( vals = validators.toSet )

    def addField[ F ](
        builder : FieldDescriptionBuilderWithoutSchema[ F ] => FieldDescription[ F ],
    )(
        implicit tup : Tupler[ F :: RV ],
    ) : ProductSchemaBuilder[ T, FieldDescription[ F ] :: R, F :: RV, AF, AFS ] = {
        val fd : FieldDescription[ F ] = builder( FieldDescriptionBuilder[ F ] )
        val newFieldDescs = fd :: fieldDescs
        copy[ T, FieldDescription[ F ] :: R, F :: RV, AF, AFS ]( desc, vals, aftSch, newFieldDescs )
    }

    def additionalFields[ F, S <: Schema[ F ] ]( implicit schema : S ) : ProductSchemaBuilder[ T, R, RV, F, S ] = {
        copy[ T, R, RV, F, S ]( desc, vals, schema, fieldDescs )
    }

    def buildAdditionalFieldsSchema[ F, S <: Schema[ F ] ]( builder : SchemaBuilder[ F ] => S ) : ProductSchemaBuilder[ T, R, RV, F, S ] = {
        copy[ T, R, RV, F, S ]( desc, vals, builder( SchemaBuilder[ F ] ) )
    }

    def construct(
        constructor : ( tupler.Out, Map[ String, AF ] ) => T,
    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
        ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ),
        )

    def deconstruct(
        deconstructor : T => (tupler.Out, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ tupler.Out, RV ],
    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
        ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            ( value : T ) => {
                val (tupleRes, afs) = deconstructor( value )
                val rvRes = detup.to( tupleRes )
                (rvRes, afs)
            }
        )
}

case class ProductSchemaBuilderWithConstructor[ T, R <: HList, RV <: HList, AF, AFS <: Schema[ AF ] ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : AFS,
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler[ RV ],
) {
    def description( description : String ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
        copy( vals = validators.toSet )

    def construct(
        constructor : ( tupler.Out, Map[ String, AF ] ) => T,
    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS ] =
        copy( constr = ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ) )

    def deconstruct(
        deconstructor : T => (tupler.Out, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ tupler.Out, RV ],
    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            constr,
            ( value : T ) => {
                val (tupleRes, afs) = deconstructor( value )
                val rvRes = detup.to( tupleRes )
                (rvRes, afs)
            }
        )
}

case class ProductSchemaBuilderWithDeconstructor[ T, R <: HList, RV <: HList, AF, AFS <: Schema[ AF ] ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : AFS,
    private[ product ] val fieldDescs : R,
    private[ product ] val deconstr : T => (RV, Map[ String, AF ])
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler[ RV ],
) {
    def description( description : String ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
        copy( vals = validators.toSet )

    def deconstruct(
        deconstructor : T => (tupler.Out, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ tupler.Out, RV ],
    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS ] =
        copy( deconstr =
            ( value : T ) => {
                val (tupleRes, afs) = deconstructor( value )
                val rvRes = detup.to( tupleRes )
                (rvRes, afs)
            }
        )

    def construct(
        constructor : ( tupler.Out, Map[ String, AF ] ) => T,
    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ),
            deconstr,
        )
}

case class BuildableProductSchemaBuilder[ T, R <: HList, RV <: HList, AF, AFS <: Schema[ AF ] ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : AFS,
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T,
    private[ product ] val deconstr : T => (RV, Map[ String, AF ]),
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler[ RV ],
) {
    def description( description : String ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        copy( vals = validators.toSet )

    def build(
        implicit
        lengther : HListIntLength[ R ],
    ) : ProductSchema[ T, R, RV, AF, AFS ] =
        ProductSchema[ T, R, RV, AF, AFS ](
            desc,
            vals,
            fieldDescs,
            aftSch,
            constr,
            deconstr,
        )
}