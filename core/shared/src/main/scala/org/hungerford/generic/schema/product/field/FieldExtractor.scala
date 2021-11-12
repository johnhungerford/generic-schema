package org.hungerford.generic.schema.product.field

import scala.compiletime.{summonInline, erasedValue}
import org.hungerford.generic.schema.types.SimpleExtractor

object FieldExtractor {
  
    transparent inline def extractFromFieldDescriptions[ Source, R <: Tuple ](
        source : Source,
        fields : R,
    ) : Tuple = {
        inline fields match {
            case EmptyTuple => EmptyTuple
            case fds : ( FieldDescription.AuxS[ t, s ] *: ts) =>
                val fieldExtractor = summonInline[ SimpleExtractor[ Source, FieldDescription.AuxS[ t, s ] ] ]
                fieldExtractor.extract( source, fds.head ) *: extractFromFieldDescriptions( source, fds.tail )
        }
    }

}
