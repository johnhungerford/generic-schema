package org.hungerford.generic.schema.selector

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.coproduct.subtype.Subtype

class ComponentRetrieverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

//    behavior of "ComponentRetriever"
//
//    case class Innerest( bool : Boolean, dbl : Double )
//    case class Inner( innerest : Innerest, str : String )
//    case class Outer( int : Int, inner : Inner )
//
//    val sch = Schema.derived[ Outer ]
//
//    it should "retrieve a nested field by name" in {
//        val innerSch = Schema.derived[ Inner ]
//        val fld = ComponentRetriever.retrieve( sch )( Selector.field( "inner" ) /- "innerest" /- "dbl" )
//        summon[ fld.type <:< Field.Extr[ Innerest, Double ] ]
//        fld.fieldName shouldBe "dbl"
//        fld.description shouldBe None
//        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
//        fld.schema.shape shouldBe ()
//    }
//
//    it should "retrieve a nested field by index" in {
//        val innerSch = Schema.derived[ Inner ]
//        val fld = ComponentRetriever.retrieve( sch )( Selector.field( 1 ) /- 0 /- 1 )
//        summon[ fld.type <:< Field.Extr[ Innerest, Double ] ]
//        fld.fieldName shouldBe "dbl"
//        fld.description shouldBe None
//        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
//        fld.schema.shape shouldBe ()
//    }
//
//    it should "retrieve a nested field by type" in {
//        val innerSch = Schema.derived[ Inner ]
//        val fld = ComponentRetriever.retrieve( sch )( field( t[ Inner ] ) /- t[ Innerest ] /- t[ Double ] )
//        summon[ fld.type <:< Field.Extr[ Innerest, Double ] ]
//        fld.fieldName shouldBe "dbl"
//        fld.description shouldBe None
//        fld.validators shouldBe Set.empty[ Validator[ Double ] ]
//        fld.schema.shape shouldBe ()
//    }
//
//    sealed trait OuterT
//    final case class SubT() extends OuterT
//    sealed trait InnerT extends OuterT
//    final case class SubT1() extends InnerT
//    final case class SubT2(int: Int) extends InnerT
//    sealed trait CoreT extends InnerT
//    final case class SubT3(str : String) extends CoreT
//
//    it should "retrieve a nested subtype by name" in {
//        val sch = Schema.derived[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( sch )( Selector.subtype( "InnerT" ) /~ "CoreT" /~ "SubT3" )
//        summon[ st.type <:< Subtype[ CoreT, SubT3, Unit ] ]
//        st.typeName shouldBe "SubT3"
//        st.description shouldBe None
//        st.validators shouldBe Set.empty[ Validator[ SubT3 ] ]
//        st.schema.shape.fieldDescriptions.head.fieldName shouldBe "str"
//    }
//
//    it should "retrieve a nested subtype by index" in {
//        val sch = Schema.derived[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( sch )( Selector.subtype( 1 ) /~ 2 /~ 0 )
//        summon[ st.type <:< Subtype[ CoreT, SubT3, Unit ] ]
//        st.typeName shouldBe "SubT3"
//        st.description shouldBe None
//        st.validators shouldBe Set.empty[ Validator[ SubT3 ] ]
//        st.schema.shape.fieldDescriptions.head.fieldName shouldBe "str"
//    }
//
//    it should "retrieve a nested subtype by type" in {
//        val sch = Schema.derived[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( sch )( subtype( t[ InnerT ] ) /~ t[ CoreT ] /~ t[ SubT3 ] )
//        summon[ st.type <:< Subtype[ CoreT, SubT3, Unit ] ]
//        st.typeName shouldBe "SubT3"
//        st.description shouldBe None
//        st.validators shouldBe Set.empty[ Validator[ SubT3 ] ]
//        st.schema.shape.fieldDescriptions.head.fieldName shouldBe "str"
//    }
//
//    it should "retrieve a subtype from a schema builder by name" in {
//        val schBuilder = Schema.derivedBuilder[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( schBuilder )( Selector.subtype( "InnerT" ) )
//        st.typeName shouldBe "InnerT"
//    }
//
//    it should "retrieve a subtype from a schema builder by index" in {
//        val schBuilder = Schema.derivedBuilder[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( schBuilder )( Selector.subtype( 1 ) )
//        st.typeName shouldBe "InnerT"
//    }
//
//    it should "retrieve a subtype from a schema builder by type" in {
//        val schBuilder = Schema.derivedBuilder[ OuterT ]
//
//        val st = ComponentRetriever.retrieve( schBuilder )( subtype( t[ InnerT ] ) )
//        st.typeName shouldBe "InnerT"
//    }
//
//    case class SimpleRecur( sr : SimpleRecur )
//    val srSch = Schema.derived[ SimpleRecur ]
//
//    it should "retrieve a simple recursive field one level past the lazy field" in {
//        import srSch.givenSchema
//        val res = srSch( "sr" / "sr" ).schema
//        res shouldBe srSch
//    }
//
//    it should "retrieve a simple recursive field N levels past the lazy field" in {
//        import srSch.givenSchema
//        val res = srSch( "sr" / "sr" / "sr" / "sr" / "sr" / "sr" ).schema
//        res shouldBe srSch
//    }
//
//    case class OuterRecur( inner : InnerRecur )
//    case class InnerRecur( self : InnerRecur )
//    val orSch = Schema.derived[ OuterRecur ]
//
//    it should "retrieve a nested recursive field one level past the lazy field" in {
//        val iSch = orSch( "inner" ).schema
//        import iSch.givenSchema
//        val res = orSch( "inner" / "self" / "self" ).schema
//        res shouldBe iSch
//    }
//
//    it should "retrieve a nested recursive field N levels past the lazy field" in {
//        val iSch = orSch( "inner" ).schema
//        import iSch.givenSchema
//        val res = orSch( "inner" / "self" / "self" / "self" / "self" / "self" / "self" ).schema
//        res shouldBe iSch
//    }
//
//    case class OutRec( inner1 : Inner1 )
//    sealed trait Inner1
//    case object Inner1a extends Inner1
//    final case class Inner1b( inner2 : Inner2 ) extends Inner1
//    case object Inner1c extends Inner1
//    case class Inner2( inner3 : Inner3 )
//    case class Inner3( inner1 : Inner1 )
//
//
//    it should "retrieve components past a highly nested lazy field" in {
//        val outRecSch = Schema.derived[ OutRec ]
//
//        val iSch = outRecSch( "inner1" ).schema
//        val i2Sch = outRecSch( "inner1" / "Inner1b" / "inner2" ).schema
//        import i2Sch.givenSchema
//        import iSch.givenSchema
//        val res1 = outRecSch( "inner1" / "Inner1b" / "inner2" / "inner3" / "inner1" ).schema
//        res1 shouldBe iSch
//        val res2 = outRecSch( "inner1" / "Inner1b" / "inner2" / "inner3" / "inner1" / "Inner1b" / "inner2" ).schema
//        res2 shouldBe i2Sch
//    }


}
