package org.hungerford.generic.schema.product.constructor

import org.hungerford.generic.schema.types.Last
import org.hungerford.generic.schema.product.LowestPriorityDeconstructorChoosers

trait ProductConstructor[ C, R <: Tuple, AF, T ] {
    def convert( from : C ) : (R, Map[ String, AF ]) => T

    def construct(
        from : C,
    )(
        fields : R,
        additionalFields : Map[ String, AF ] = Map.empty[ String, AF ],
    ) : T = convert( from )( fields, additionalFields )
}

trait LowestPriorityProductConstructors {
    given standard[ R <: Tuple, AF, T ] : ProductConstructor[ (R, Map[ String, AF ]) => T, R, AF, T ] with {
        override def convert( from : (R, Map[ String, AF ]) => T ) : (R, Map[ String, AF ]) => T = from
    }
}

trait LowPriorityProductConstructors extends LowestPriorityProductConstructors {
    given singleField[ F, T, AF ] : ProductConstructor[ (F, Map[ String, AF ]) => T, F *: EmptyTuple, AF, T ] with {
        override def convert( from : (F, Map[ String, AF ]) => T ) : (F *: EmptyTuple, Map[ String, AF ]) => T = {
            ( fields : F *: EmptyTuple, additionalFields : Map[ String, AF ] ) => from( fields.head, additionalFields )
        }
    }
}

object ProductConstructor extends LowPriorityProductConstructors {
    given standardWithoutAF[ R <: Tuple, T ] : ProductConstructor[ R => T, R, Nothing, T ] with {
        override def convert( from : R => T ) : (R, Map[ String, Nothing ]) => T = {
            ( fields : R, _ : Map[ String, Nothing ] ) => from( fields )
        }
    }

    given singleFieldWithoutAF[ F, T ] : ProductConstructor[ F => T, F *: EmptyTuple, Nothing, T ] with {
        override def convert( from : F => T ) : (F *: EmptyTuple, Map[ String, Nothing ]) => T = {
            ( fields : F *: EmptyTuple, _ : Map[ String, Nothing ] ) => from( fields.head )
        }
    }
}

trait ProductDeconstructor[ DC, R <: Tuple, AF, T ] {
    def convert( from : DC ) : T => (R, Map[ String, AF ])

    def deconstruct( from : DC )(
        value : T,
    ) : (R, Map[ String, AF ]) = convert( from )( value )
}

trait LowestPriorityProductDeconstructors {
    given standard[ T, R <: Tuple, AF ] :
        ProductDeconstructor[ T => (R, Map[ String, AF ]), R, AF, T ] with {
        def convert( from : T => (R, Map[ String, AF ]) ) :
            T => (R, Map[ String, AF ]) = from
    }
}

trait LowPriorityProductDeconstructors extends LowestPriorityProductDeconstructors {
    given singleField[ F, T, AF ] : ProductDeconstructor[ T => (F, Map[ String, AF ]), F *: EmptyTuple, AF, T ] with {
        def convert( from : T => (F, Map[ String, AF ]) ) : T => (F *: EmptyTuple, Map[ String, AF ]) = {
            ( value : T ) => {
                val (field, additionalFields) = from( value )
                (field *: EmptyTuple, additionalFields)
            }
        }
    }
}

object ProductDeconstructor extends LowPriorityProductDeconstructors {

    given noAF[ T, R <: Tuple ] : ProductDeconstructor[ T => R, R, Nothing, T ] with {
        def convert( from : T => R ) : T => (R, Map[ String, Nothing ]) = {
            ( value : T ) => {
                val res : R = from( value )
                (res, Map.empty[ String, Nothing ])
            }
        }
    }

    given tupled[ T, R <: Tuple, AF, Res <: Tuple ](
        using
        ls : Last.Aux[ Res, R, Map[ String, AF ] ],
    ) : ProductDeconstructor[ T => Res, R, AF, T ] with {
        def convert( from : T => Res ) : T => (R, Map[ String, AF ]) = {
            ( value : T ) => {
                val res : Res = from( value )
                ls.last( res )
            }
        }
    }

    given singleFieldNoAF[ F, T ] : ProductDeconstructor[ T => F, F *: EmptyTuple, Nothing, T ] with {
        def convert( from : T => F ) : T => (F *: EmptyTuple, Map[ String, Nothing ]) = {
            ( value : T ) => {
                (from( value ) *: EmptyTuple, Map.empty[ String, Nothing ])
            }
        }
    }

}
