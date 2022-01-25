package org.hungerford.generic.schema.product.field

import scala.annotation.tailrec
import scala.compiletime.{erasedValue, summonInline}

trait FieldInjector[ T, F, N <: FieldName, S, Target ] {    
    def inject( field : Field.Aux[ T, F, N, S ], value : F, into : Target ) : Target
}

object FieldInjector {
    transparent inline def inject[ R <: Tuple, RV <: Tuple, Target ](
        fieldDescriptions : R,
        fieldValues : RV,
        into : Target,
    ) : Any = inline fieldDescriptions match {
        case _ : EmptyTuple => into
        case fds : ( Field.Aux[ t, f, n, s ] *: fs) =>
            val injector = summonInline[ FieldInjector[ t, f, n, s, Target ] ]
            type T = t
            type F = f
            inline fieldValues match {
                case fvs : (F *: vts ) =>
                    val thisRes = injector.inject( fds.head, fvs.head, into )
                    inject( fds.tail, fvs.tail, thisRes )
                
            }
    }
}
