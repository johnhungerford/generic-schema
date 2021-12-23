package org.hungerford.generic.schema.tapir

trait TapirSchemaTranslation
  extends TapirSchemaProductTranslation
    with TapirSchemaCoproductTranslation

object TapirSchemaTranslation
  extends TapirSchemaTranslation
