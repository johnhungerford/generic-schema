package org.hungerford.generic.schema.upikle

import org.hungerford.generic.schema.translation.BiMapProductSchemaTranslation
import org.hungerford.generic.schema.product.field.TranslatedFieldDescription
import ujson.Value
import upickle.default._

import scala.collection.immutable.ListMap
import scala.util.Try

object UPickleSchemaTranslation extends BiMapProductSchemaTranslation[ ReadWriter, Value.Value, ListMap[ String, Value.Value ] ] {

    /**
     * Construct a schema from the two parts of a bimap.
     *
     * @param to   T => MapVal : writer
     * @param from MapVal => T : reader
     * @tparam T type being read/written
     * @return type class instance for some reader/writer
     */
    override def schemaFromBimap[ T ]( to : T => Value, from : Value => T ) : ReadWriter[ T ] = {
        readwriter[ Value.Value ].bimap[ T ]( to, from )
    }

    /**
     * Initial empty value for the type being mapped to and from, that can be built
     * by adding field values. For instance, if the value type is Map[ String, T ],
     * initMapVal would be Map.empty[ String, T ]
     *
     * @return initial value of buildable bimap type
     */
    override def initMapVal : ListMap[ String, Value.Value ] = ListMap.empty

    /**
     * Construct the final type to be bimapped to and from
     *
     * @param buildableValue
     * @return
     */
    override def buildMapVal( buildableValue : ListMap[ String, Value.Value ] ) : Value.Value = {
        val bvSeq : Seq[ (String, Value) ] = buildableValue.toSeq
        ujson.Obj( bvSeq.head, bvSeq : _* )
    }

    override def extractField[ T ](
        from : Value,
        using : TranslatedFieldDescription[ T, ReadWriter ],
    ) : T = {
        val fieldValue : Value = from.obj( using.fieldName )
        val rw : ReadWriter[ T ] = using.schema
        read[ T ]( fieldValue )( rw )
    }

    override def extractAdditionalFields[ T ]( from : Value, using : ReadWriter[ T ] ) : Map[ String, T ] = {
        from.obj.toMap collect {
            case (fieldName, value) if Try( read[ T ]( value )( using ) ).toOption.nonEmpty =>
                fieldName -> read[ T ]( value )( using )
        }
    }

    override def writeField[ T ]( value : T, to : ListMap[ String, Value.Value ], using : TranslatedFieldDescription[ T, ReadWriter ] ) : ListMap[ String, Value ] = {
        val valueJson : Value.Value = writeJs( value )( using.schema )
        to + ( (using.fieldName, valueJson) )
    }

    override def writeAdditionalFields[ T ]( from : Map[ String, T ], to : ListMap[ String, Value ], using : ReadWriter[ T ] ) : ListMap[ String, Value ] = {
        from.mapValues( v => writeJs( v )( using ) )
        from.foldLeft( to )( (oldFields : ListMap[ String, Value ], nextField : (String, T) ) => {
            val (fieldName, fieldValue) = nextField
            oldFields + (fieldName -> writeJs( fieldValue )( using ))
        } )
    }

}
