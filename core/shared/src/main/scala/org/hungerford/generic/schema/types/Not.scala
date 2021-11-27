package org.hungerford.generic.schema.types

sealed trait Not[ T ]

trait LowPriorityNotInstances {
    given not[ T ] : Not[ T ] with {}
}

object Not extends LowPriorityNotInstances {
    given notNot1[ T ]( using t : T ) : Not[ T ] with { ??? }
    given notNot2[ T ]( using t : T ) : Not[ T ] with { ??? }
}
