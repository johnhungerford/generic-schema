package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Primitive
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.language.higherKinds

abstract class PrimitiveSchemaTranslatorTest[ OtherSchema[ _ ] ](
   using
   intSchema : OtherSchema[ Int ],
) extends AnyFlatSpecLike with Matchers {

   behavior of "SchemaTranslator"

   it should "translate an integer primitive" in {
       assertCompiles( "SchemaTranslator.translate( Primitive[ Int ]() )" )
   }

}
