package org.hungerford.generic.schema.product.field

import scala.annotation.tailrec
import scala.compiletime.{erasedValue, summonInline}

trait FieldInjector[ T, F, N <: FieldName, S, Target ] {    
    def inject( field : Field.Aux[ T, F, N, S ], value : T, into : Target ) : Target
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

trait TranslatedFieldInjector[ T, Target, OtherSchema[ _ ] ] {    
    def inject( field : TranslatedFieldDescription[ T, OtherSchema ], value : T, into : Target ) : Target
}

object TranslatedFieldInjector {
    inline def inject[ R <: Tuple, RV <: Tuple, Target, OtherSchema[ _ ] ](
        fieldDescriptions : R,
        fieldValues : RV,
        into : Target,
    ) : Target = inline fieldDescriptions match {
        case _ : EmptyTuple => into
        case fds : (TranslatedFieldDescription[ t, OtherSchema ] *: ts) =>
            type T = t
            val injector = summonInline[ TranslatedFieldInjector[ T, Target, OtherSchema ] ]
            inline fieldValues match {
                case fvs : (T *: vts ) =>
                    val thisRes = injector.inject( fds.head, fvs.head, into )
                    inject[ ts, vts, Target, OtherSchema ]( fds.tail, fvs.tail, thisRes )
                
            }
        case v =>
            println( v )
            throw Exception( "didn't match accepted fieldDescriptions type" )

    }
}
