package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.selector.Selector

class DefaultTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Default profile"

    case class Core( int : Int, str : String, bool : Boolean )
    case class Inner( core : Core )
    case class Outer( inner : Inner )

    it should "do everything I want it to" in {
        import Default.dsl.{given, *}

        val sch1 = Schema.derivedBuilder[ Outer ].build
        Schema.productBuilder[ Outer ]
          .addField( Field.fromSchema[ Outer, Inner ]( "inner_field", _.inner )( using Schema.derived[ Inner ] ) )
          .construct( Outer.apply )
          .build

        val updated = sch1.modifyComponent( "inner" )(
            _.withName( "inner_field" )
              .modifySchema(
                  _.modifyComponent( "core" )(
                      _.withName( "core_field" )
                        .modifySchema(
                            _.modifyComponent( "bool" )(
                                _.withName( "boolean_field" )
                            ),
                        ),
                  ),
              ),
        )

        val updated1 = sch1.modifyComponent( "inner" / "core" / "bool" )( _.withName( "boolean_field" ) )
        val updated2 = sch1.modifyComponent( 0 /- 0 / 2 )( _.withName( "boolean_field" ) )
        val updated3 = sch1.modifyComponent( t[ Inner ] / t[ Core ] / t[ Boolean ] )( _.withName( "boolean_field" ) )

        updated1 shouldBe updated2

        updated( "inner_field" / "core_field" / "boolean_field" ).fieldName shouldBe "boolean_field"
        updated1( "inner" / "core" / "boolean_field" ).fieldName shouldBe "boolean_field"
        updated2( "inner" / "core" / "boolean_field" ).fieldName shouldBe "boolean_field"
        updated3( "inner" / "core" / "boolean_field" ).fieldName shouldBe "boolean_field"
    }

}
