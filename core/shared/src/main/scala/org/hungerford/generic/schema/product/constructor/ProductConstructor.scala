package org.hungerford.generic.schema.product.constructor

import org.hungerford.generic.schema.types.Last

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
