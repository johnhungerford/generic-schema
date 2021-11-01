package org.hungerford.generic.schema

trait SchemaBuilder[ T ]

object SchemaBuilder {
    def empty[ T ] : SchemaBuilder[ T ] = new SchemaBuilder[T] {}
}
