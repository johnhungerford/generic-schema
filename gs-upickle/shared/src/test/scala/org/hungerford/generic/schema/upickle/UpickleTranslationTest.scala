package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.translation.{CoproductJsonTranslationTest, PrimitiveSchemaTranslatorTest, ProductJsonTranslationTest, Rec, RecursiveProduct, SchemaTranslator, SingletonJsonTranslationTest, Term, Terminal}
import ujson.Value
import upickle.default.*

import scala.collection.immutable.ListMap
import scala.util.Try
import UPickleSchemaTranslation.given
import upickle.default

class UPicklePrimitiveTranslationTest
  extends PrimitiveSchemaTranslatorTest[ ReadWriter ]

class UpickleProductTranslationTest
  extends ProductJsonTranslationTest[ ReadWriter ] {

    import org.hungerford.generic.schema.translation.ProductTranslationTestSchemata.*

    def osNoAf: ReadWriter[ NoAF ] = SchemaTranslator.translate( noAfSchema )
    def osHasAf: ReadWriter[ HasAF ] = SchemaTranslator.translate( hasAfSchema )
    def osHasAfPrim: ReadWriter[ HasAF ] = SchemaTranslator.translate( hasAfPrimitiveSch )
    def osOutside: ReadWriter[ Outside ] = SchemaTranslator.translate( outsideSch )
    def osOutsideIns: ReadWriter[ Outside ] = SchemaTranslator.translate( outsideSchUsingInside )
    def osOutsideDer: ReadWriter[ Outside ] = SchemaTranslator.translate( outsideSchemaDerived )
//    def osRecursiveSchemaDer: ReadWriter[ RecursiveProduct ] = SchemaTranslator.translate( recursiveSchemaDerived )
    def osRecursiveSchemaDer: ReadWriter[ RecursiveProduct ] = readwriter[ String ].bimap[ RecursiveProduct ](
        _ => "HI",
        _ => RecursiveProduct( Term, 1000 ),
    )

    def writeJson[ T ]( value : T, schm : ReadWriter[ T ] ) : String = write[ T ]( value )( schm )
    def readJson[ T ]( value : String, schm : ReadWriter[ T ] ) : T = read[ T ]( value )( schm )
}

class UPickleCoproductSchemaTranslationTest
  extends CoproductJsonTranslationTest[ ReadWriter ] {
    import org.hungerford.generic.schema.translation.{Super, SuperT}

    override val superOs: ReadWriter[ Super ] = SchemaTranslator.translate( superSch )
    override def superTOs: ReadWriter[ SuperT ] = SchemaTranslator.translate( superTSch )
//    def recurSchOs : ReadWriter[ Rec ] = SchemaTranslator.translate( recurSch )
    def recurSchOs : ReadWriter[ Rec ] = readwriter[ String ].bimap[ Rec ]( _ => "HI", _ => Rec( Terminal, 2 ) )

    override def writeJson[ T ]( value: T, schm: ReadWriter[ T ] ): String = write[ T ]( value )( schm )

    override def readJson[ T ]( value: String, schm: ReadWriter[ T ] ): T = read[ T ]( value )( schm )
}

class UPickleSingletonSchemaTranslationTest
  extends SingletonJsonTranslationTest[ ReadWriter ] {
    import SingletonJsonTranslationTest.*

    val tsOSchema: ReadWriter[ TestSingleton.type ] = SchemaTranslator.translate( tsSchema )
    val stOSchema: ReadWriter[ SuperT ] = SchemaTranslator.translate( stSchema )

    def writeJson[ T ]( value: T, schm: ReadWriter[ T ] ): String = {
        write[ T ]( value )( schm )
    }

    def readJson[ T ]( json: String, schm: ReadWriter[ T ] ): Option[ T ] = {
        read[ T ]( json )( schm )
        Try( read[ T ]( json )( schm ) ).toOption
    }
}
