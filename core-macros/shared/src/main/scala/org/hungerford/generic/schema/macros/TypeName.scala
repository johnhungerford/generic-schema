package org.hungerford.generic.schema.macros
import scala.quoted.*

object TypeName {

    def showTypeImpl[T : Type](using Quotes) =
        val s = Type.show[T]
        Expr[s.type](s)

    transparent inline def showType[T] = ${showTypeImpl[T]}
}
