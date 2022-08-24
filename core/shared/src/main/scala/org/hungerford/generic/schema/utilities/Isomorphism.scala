package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema
import org.hungerford.generic.schema.{Schema, SchemaExtractor, SchemaProvider}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}

trait Isomorphism[ A, B ] {
    def convertForward(a: A): B
    def convertBackward(b: B): A
}

object Isomorphism {
    given isomorphismFrom[ A, B ](
        using
        migrForward: Migration[A, B],
        migrBackward: Migration[B, A],
    ) : Isomorphism[ A, B ] with {
        def convertForward(a: A): B = migrForward.migrate(a)
        def convertBackward(b: B): A = migrBackward.migrate(b)
    }
}

