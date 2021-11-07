package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldNamesCollector}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder}
import shapeless._
import shapeless.ops.hlist.{Prepend, Tupler}


case class ProductSchemaBuilder[ T, R <: HList, RV <: HList, AF, AFS, Tup ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler.Aux[ RV, Tup ],
) {
    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] = copy( vals = validators.toSet )

    def addField[ F, NewR <: HList, NewRV <: HList, NewTup, S ](
        fd : FieldDescription.Aux[ F, S ],
    )(
        implicit
        prepR : Prepend.Aux[ R, FieldDescription.Aux[ F, S ] :: HNil, NewR ],
        prepRV : Prepend.Aux[ RV, F :: HNil, NewRV ],
        tup : Tupler.Aux[ NewRV, NewTup ],
        rConstraint : CtxWrapHListsConstraint[ FieldDescription, NewR, NewRV ]
    ) : ProductSchemaBuilder[ T, NewR, NewRV, AF, AFS, NewTup ] = {
        val newFieldDescs = fieldDescs :+ fd
        copy[ T, NewR, NewRV, AF, AFS, NewTup ]( desc, vals, aftSch, newFieldDescs )
    }

    def additionalFields[ F ] : AdditionalFieldsBuilder[ T, R, RV, F, Tup ] =
        AdditionalFieldsBuilder[ T, R, RV, F, Tup ](
            desc,
            vals,
            fieldDescs,
        )

    def construct(
        constructor : ( Tup, Map[ String, AF ] ) => T,
    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ] =
        ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ),
        )

    def deconstruct(
        deconstructor : T => (Tup, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ Tup, RV ],
    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ] =
        ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ](
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

case class ProductSchemaBuilderWithConstructor[ T, R <: HList, RV <: HList, AF, AFS, Tup ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler.Aux[ RV, Tup ],
) {
    def description( description : String ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( vals = validators.toSet )

    def construct(
        constructor : ( Tup, Map[ String, AF ] ) => T,
    ) : ProductSchemaBuilderWithConstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( constr = ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ) )

    def deconstruct(
        deconstructor : T => (Tup, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ Tup, RV ],
    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ](
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

case class ProductSchemaBuilderWithDeconstructor[ T, R <: HList, RV <: HList, AF, AFS, Tup ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val deconstr : T => (RV, Map[ String, AF ])
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler.Aux[ RV, Tup ],
) {
    def description( description : String ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( vals = validators.toSet )

    def deconstruct(
        deconstructor : T => (Tup, Map[ String, AF ]),
    )(
        implicit
        detup : Generic.Aux[ Tup, RV ],
    ) : ProductSchemaBuilderWithDeconstructor[ T, R, RV, AF, AFS, Tup ] =
        copy( deconstr =
            ( value : T ) => {
                val (tupleRes, afs) = deconstructor( value )
                val rvRes = detup.to( tupleRes )
                (rvRes, afs)
            }
        )

    def construct(
        constructor : ( Tup, Map[ String, AF ] ) => T,
    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ](
            desc,
            vals,
            aftSch,
            fieldDescs,
            ( rv : RV, afs : Map[ String, AF ] ) => constructor( tupler( rv ), afs ),
            deconstr,
        )
}

case class BuildableProductSchemaBuilder[ T, R <: HList, RV <: HList, AF, AFS, Tup ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Schema.Aux[ AF, AFS ],
    private[ product ] val fieldDescs : R,
    private[ product ] val constr : ( RV, Map[ String, AF ] ) => T,
    private[ product ] val deconstr : T => (RV, Map[ String, AF ]),
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler.Aux[ RV, Tup ],
) {
    def description( description : String ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] =
        copy( vals = validators.toSet )

    def build(
        implicit
        lengther : HListIntLength[ R ],
        fns : FieldNamesCollector[ R ],
    ) : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, Tup ] ] =
        ComplexSchema(
            ProductShape[ T, R, RV, AF, AFS, Tup ](
                fieldDescriptions = fieldDescs,
                additionalFieldsSchema = aftSch,
                constructor = constr,
                deconstructor = deconstr,
            ),
            genericDescription = desc,
            genericValidators = vals,
        )
}

case class AdditionalFieldsBuilder[ T, R <: HList, RV <: HList, AF, Tup ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val fieldDescs : R,
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler.Aux[ RV, Tup ],
) {
    def fromSchema[ S ](
        implicit schema : Schema.Aux[ AF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, AF, S, Tup ] = {
        ProductSchemaBuilder[ T, R, RV, AF, S, Tup ](
            desc,
            vals,
            schema,
            fieldDescs,
        )
    }

    def buildSchema[ S ](
        builder : SchemaBuilder[ AF ] => Schema.Aux[ AF, S ],
    ) : ProductSchemaBuilder[ T, R, RV, AF, S, Tup ] = {
        fromSchema( builder( SchemaBuilder[ AF ] ) )
    }
}

object ProductSchemaBuilder {
    def from[ T, R <: HList, RV <: HList, AF, AFS, Tup ](
        schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, Tup ] ],
    )(
        implicit
        fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
        tupler : Tupler.Aux[ RV, Tup ],
    ) : BuildableProductSchemaBuilder[ T, R, RV, AF, AFS, Tup ] = {
        BuildableProductSchemaBuilder(
            schema.genericDescription,
            schema.genericValidators,
            schema.shape.additionalFieldsSchema,
            schema.shape.fieldDescriptions,
            schema.shape.constructor,
            schema.shape.deconstructor
        )
    }
}
