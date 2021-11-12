package org.hungerford.generic.schema.product.field

import scala.compiletime.{erasedValue, summonInline}

trait FieldInjector[ T, S, Target ] {    
    def inject( field : FieldDescription.AuxS[ T, S ], value : T, into : Target ) : Target
}

object FieldInjector {
    transparent inline def inject[ R <: Tuple, RV <: Tuple, Target ](
        fieldDescriptions : R,
        fieldValues : RV,
        into : Target,
    ) : Any = inline fieldDescriptions match {
        case _ : EmptyTuple => into
        case fds : (FieldDescription.AuxS[ t, s ] *: ts) =>
            val injector = summonInline[ FieldInjector[ t, s, Target ] ]
            type T = t
            inline fieldValues match {
                case fvs : (T *: vts ) =>
                    val thisRes = injector.inject( fds.head, fvs.head, into )
                    inject( fds.tail, fvs.tail, thisRes )
                
            }
    }
}

trait TranslatedFieldInjector[ T, Target, OtherSchema[ _ ] ] {    
    def inject( field : TranslatedFieldDescription[ T, OtherSchema ], value : T, into : Target ) : Target
}

object TranslatedFieldInjector {
    transparent inline def inject[ R <: Tuple, RV <: Tuple, Target, OtherSchema[ _ ] ](
        fieldDescriptions : R,
        fieldValues : RV,
        into : Target,
    ) : Any = inline fieldDescriptions match {
        case _ : EmptyTuple => into
        case fds : (TranslatedFieldDescription[ t, OtherSchema ] *: ts) =>
            val injector = summonInline[ TranslatedFieldInjector[ t, Target, OtherSchema ] ]
            type T = t
            inline fieldValues match {
                case fvs : (T *: vts ) =>
                    val thisRes = injector.inject( fds.head, fvs.head, into )
                    inject[ ts, vts, Target, OtherSchema ]( fds.tail, fvs.tail, thisRes )
                
            }

        case fds : (FieldDescription.AuxS[ t, s ] *: ts) =>
            val trans = summonInline[ FieldTranslator[ t, s, OtherSchema ] ]
            val fd = trans.translate( fds.head )
            val injector = summonInline[ TranslatedFieldInjector[ t, Target, OtherSchema ] ]
            type T = t
            inline fieldValues match {
                case fvs : (T *: vts ) =>
                    val thisRes = injector.inject( fd, fvs.head, into )
                    inject[ ts, vts, Target, OtherSchema ]( fds.tail, fvs.tail, thisRes )
                
            }

    }
}
