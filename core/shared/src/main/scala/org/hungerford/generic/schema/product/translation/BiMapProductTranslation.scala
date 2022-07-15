package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldInjector, FieldName}
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Injector, SimpleExtractor}

trait BiMapProductTranslation[ RW[ _ ], DecoderSch[ _ ], EncoderSch[ _ ], Source, Sink ]
  extends ProductDecoderTranslation[ DecoderSch, Source ]
    with ProductEncoderTranslation[ EncoderSch, Sink ] {

    def buildProductSchema[ T ]( enc : EncoderSch[ T ], dec : DecoderSch[ T ] ) : RW[ T ]

}
