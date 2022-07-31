package org.hungerford.generic.schema.types

import sun.reflect.generics.tree.TypeTree

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.compiletime.constValue
import scala.quoted.*

trait TypeName[ T ] {
    type Name <: String & Singleton

    def name: Name
}

object TypeName {
    type Aux[ T, N <: String & Singleton ] = TypeName[ T ] { type Name = N }

    transparent inline def getTypeName[T] = ${org.hungerford.generic.schema.macros.TypeName.showTypeImpl[T]}

    inline given fromMirror[ T ](
        using
        mir : Mirror { type MirroredType = T; type MirroredLabel <: String & Singleton },
    ) : TypeName[T] = {
        val nameVal : mir.MirroredLabel = constValue[ mir.MirroredLabel ]

        new TypeName[ T ]:
            type Name = mir.MirroredLabel
            def name : mir.MirroredLabel = nameVal
    }
}
