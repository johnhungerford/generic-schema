package org.hungerford.generic.schema.tapir

import scala.reflect.ClassTag

object ClassNameProvider {

    private def fixName(fullName: String): String =
        fullName.split("""\.""").lastOption.getOrElse( "NAMELESS" )
          .split("""\$""").lastOption.getOrElse( "NAMELESS" )

    def className[T: ClassTag] : String =
        val name = summon[ClassTag[T]].runtimeClass.getName
        fixName(name)

    extension [T: ClassTag](obj: T)
        def className: String = ClassNameProvider.className[T]
}
