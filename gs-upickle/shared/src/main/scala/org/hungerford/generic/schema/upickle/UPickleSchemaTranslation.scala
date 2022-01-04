package org.hungerford.generic.schema.upickle

trait UPickleSchemaTranslation
  extends UPickleProductTranslation
    with UPickleSingletonSchemaTranslation
    with UPickleCoproductSchemaTranslation {

}

object UPickleSchemaTranslation
  extends UPickleSchemaTranslation
