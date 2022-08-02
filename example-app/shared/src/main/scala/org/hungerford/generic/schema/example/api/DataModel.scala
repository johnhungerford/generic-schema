package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Schema}

object DataModel {

    case class Request( name : String, id : String, amount : Int, payment : Payment )

    sealed trait Payment
    final case class CreditCard( number : Int, expDat : Date ) extends Payment
    case object Cash extends Payment
    final case class Check( checkNumber : Int, memo : String, date : Date ) extends Payment
    case object Other extends Payment
    case class ExRequest(req : Request) extends Payment

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
}
