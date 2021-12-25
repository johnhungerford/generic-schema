package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Default, Schema}

object DataModel {
    case class Request( name : String, id : String, amount : Int, payment : Payment )

    sealed trait Payment
    final case class CreditCard( number : Int, expDat : Date ) extends Payment
    case object Cash extends Payment
    final case class Check( checkNumber : Int, memo : String, date : Date ) extends Payment

    case class Date( year : Int, month : Int )

    sealed trait Response { def status : JobStatus }
    case object TransactionPending extends Response { val status : JobStatus = Pending }
    final case class TransactionResult( id : String, amount : Int, message : Option[ String ] ) extends Response {
        val status : JobStatus = Failed
    }
    final case class TransactionFailure( failureId : String, message : Option[ String ] ) extends Response {
        val status : JobStatus = Complete
    }

    sealed trait JobStatus
    case object Pending extends JobStatus
    case object Failed extends JobStatus
    case object Complete extends JobStatus

    val requestSchema = Default.usingDsl( dsl => {
        import dsl.{*, given}

        val dateSch = Schema.derived[ Date ]
          .modifyComponent( "year" )(
              _.withValidators( Validator.min( 2000 ), Validator.max( 3000 ) )
          )
          .modifyComponent( "month" )(
              _.withValidators( Validator.min( 1 ), Validator.max( 12 ) )
          )
        import dateSch.givenSchema

        Schema.derived[ Request ]
    } )
}
