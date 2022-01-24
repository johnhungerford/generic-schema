//package org.hungerford.generic.schema.selector
//
//import org.hungerford.generic.schema.product.field.FieldBuilder
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.hungerford.generic.schema.Schema
//import org.hungerford.generic.schema.Default.dsl.*
//import org.hungerford.generic.schema.coproduct.subtype.SubtypeBuilder
//
//class ComponentUpdaterTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
//
//    behavior of "ComponentUpdater - fields"
//
//    case class One( str : String )
//    val oneSch = Schema.derived[ One ]
//    case class Two( one : One )
//    val twoSch = Schema.derived[ Two ]
//    case class Three( two : Two, str : String )
//    case class Four( int : Int, three : Three )
//    case class Five( dbl : Double, map : Map[ Int, String ], four : Four )
//    val fiveSch = Schema.derived[ Five ]
//
//    it should "update a schema's field using a selector" in {
//        val newSch = ComponentUpdater.update( oneSch )( Selector.field( "str" ) ){ field =>
//            FieldBuilder.from( field )
//              .fieldName( "string_field" )
//              .build
//        }
//
//        newSch.shape.fieldNames shouldBe Set( "string_field" )
//    }
//
//    it should "update a schema's nested field using a selector" in {
//        val newSch = ComponentUpdater.update( twoSch )( Selector.field( "one" ) / "str" ){ field =>
//            FieldBuilder.from( field )
//              .fieldName( "string_field_2" )
//              .build
//        }
//
//        newSch.shape.fieldNames shouldBe Set( "one" )
//        newSch.shape.fieldDescriptions.head.schema.shape.fieldNames shouldBe Set( "string_field_2" )
//    }
//
//    it should "update a highly nested field using a selector" in {
//        val newSch = ComponentUpdater.update( fiveSch )( Selector.field( "four" ) / "three" / "two" / "one" / "str" ) { field =>
//            FieldBuilder.from( field )
//              .fieldName( "string_field_5" )
//              .build
//        }
//
//        val fourS = newSch.shape.fieldDescriptions.tail.tail.head.schema
//        val threeS = fourS.shape.fieldDescriptions.tail.head.schema
//        val twoS = threeS.shape.fieldDescriptions.head.schema
//        val oneS = twoS.shape.fieldDescriptions.head.schema
//        oneS.shape.fieldDescriptions.head.fieldName shouldBe "string_field_5"
//    }
//
//    it should "update a highly nest field using an ambiguous selector" in {
//        import Selector.given
//        val newSch = ComponentUpdater.update( fiveSch )( "four" / "three" / "two" / "one" / "str" ) { field =>
//            FieldBuilder.from( field )
//              .fieldName( "string_field_5" )
//              .build
//        }
//
//        val fourS = newSch.shape.fieldDescriptions.tail.tail.head.schema
//        val threeS = fourS.shape.fieldDescriptions.tail.head.schema
//        val twoS = threeS.shape.fieldDescriptions.head.schema
//        val oneS = twoS.shape.fieldDescriptions.head.schema
//        oneS.shape.fieldDescriptions.head.fieldName shouldBe "string_field_5"
//    }
//
//    behavior of "ComponentUpdater - subtypes"
//
//    sealed trait OuterT
//    final case class SubT() extends OuterT
//    sealed trait InnerT extends OuterT
//    final case class SubT1() extends InnerT
//    final case class SubT2(int: Int) extends InnerT
//    sealed trait CoreT extends InnerT
//    final case class SubT3(str : String) extends CoreT
//
//    it should "update a subtype" in {
//        val sch = Schema.derived[ OuterT ]
//
//        val newSch = ComponentUpdater.update( sch )( Selector.subtype( "InnerT" ) ) { subtype =>
//            SubtypeBuilder.from( subtype )
//              .typeName( "NEW-NAME" )
//              .fromSchema( Schema.primitive[ InnerT ] )
//              .build
//        }
//
//        newSch.shape.subtypeDescriptions.size shouldBe 2
//        newSch.shape.subtypeDescriptions.head.typeName shouldBe "SubT"
//        newSch.shape.subtypeDescriptions.tail.head.typeName shouldBe "NEW-NAME"
//        newSch.shape.subtypeDescriptions.tail.head.schema.shape shouldBe ()
//    }
//
//    it should "update from a subtype" in {
//        val st = Schema.derived[ OuterT ]( "InnerT" )
//
//        st.schema.shape.subtypeDescriptions.tail.head.typeName shouldBe "SubT2"
//        st.schema.shape.subtypeDescriptions.tail.head.schema.shape should not be( () )
//
//        val newSt = ComponentUpdater.update( st )( Selector.subtype( "SubT2" ) ) { subtype =>
//            SubtypeBuilder.from( subtype )
//              .typeName( "NEW-NAME" )
//              .fromSchema( Schema.primitive )
//              .build
//        }
//
//        newSt.schema.shape.subtypeDescriptions.size shouldBe 3
//        newSt.schema.shape.subtypeDescriptions.head.typeName shouldBe "SubT1"
//        newSt.schema.shape.subtypeDescriptions.tail.head.typeName shouldBe "NEW-NAME"
//        newSt.schema.shape.subtypeDescriptions.tail.head.schema.shape shouldBe ()
//    }
//
//    it should "update from a coproduct schema builder" in {
//        val schBuilder = Schema.derivedBuilder[ OuterT ]
//
//        val newSch = ComponentUpdater.update( schBuilder )( Selector.subtype( "InnerT" ) ) { subtype =>
//            SubtypeBuilder.from( subtype )
//              .typeName( "NEW-NAME" )
//              .fromSchema( Schema.primitive[ InnerT ] )
//              .build
//        }
//
//        newSch.sts.size shouldBe 2
//        newSch.sts.head.typeName shouldBe "SubT"
//        newSch.sts.tail.head.typeName shouldBe "NEW-NAME"
//        newSch.sts.tail.head.schema.shape shouldBe ()
//    }
//
//}
