package org.hungerford.generic.schema.singleton

import scala.deriving.Mirror
import scala.compiletime.constValue
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.types.Sub

trait SingletonDeriver[ T ] {
    type Out

    def derive : Out
}

object SingletonDeriver {
    type Aux[ T <: Singleton, O ] = SingletonDeriver[ T ] { type Out = O }

    given mirrorDeriver[ T <: Singleton ](
        using
        mir : Mirror.ProductOf[ T ],
        ev : mir.type <:< Mirror.Singleton,
        ev2 : Sub[ mir.MirroredLabel, TypeName ],
        voN : ValueOf[ mir.MirroredLabel ],
        voT : ValueOf[ mir.MirroredMonoType ],
    ) : SingletonDeriver.Aux[ T, SingletonShape[ T, ev2.ASub ] ] = {
        val name = ev2( voN.value )
        val value = voT.value
        new SingletonDeriver[ T ] {
            type Out = SingletonShape[ T, ev2.ASub ]

            override def derive: Out = SingletonShape[ T, ev2.ASub ]( name, value )
        }
    }
}
