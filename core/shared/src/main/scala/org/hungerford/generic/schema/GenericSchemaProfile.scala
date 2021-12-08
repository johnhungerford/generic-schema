package org.hungerford.generic.schema

case class SchemaConfiguration()

trait GenericSchemaProfile {

    def configure : SchemaConfiguration

    final lazy val dsl : GenericSchemaDsl = new GenericSchemaDsl( configure )

    def usingDsl[ T ]( withDsl : GenericSchemaDsl => SchemaConfiguration ?=> T ) : T =
        withDsl( dsl )( using configure )

}

trait Default extends GenericSchemaProfile {
    val configure : SchemaConfiguration = SchemaConfiguration()
}

object Default extends Default
