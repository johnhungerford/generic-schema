package org.hungerford.generic.schema.tapir

trait TapirSchemaTranslation
  extends TapirSchemaProductTranslation
    with TapirSchemaCoproductTranslation
    with TapirSchemaSingletonTranslation

object TapirSchemaTranslation
  extends TapirSchemaTranslation
