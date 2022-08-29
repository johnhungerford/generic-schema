package org.hungerford.generic.schema.example.api

import sttp.tapir.ztapir._
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zhttp.http.{Http, Request as ZHRequest, Response as ZHResponse}
import zio._

import DataModel.*

object RestServer {
    def dateString(date: Date): String =
        s"${date.month}/${date.year}"

    val transactionZEndpoint =
        RestApi.transactionEndpoint.zServerLogic[Any]((req: Request) => {
            ZIO.succeed(TransactionResult(req.id + "_res", req.amount * 2, req.payment match {
                case CreditCard( number, expDat ) => Some( s"Credit card: ${number}, ${dateString(expDat)}" )
                case DataModel.Cash => Some( "Cash" )
                case Check( checkNumber, memo, date ) => Some( s"Check: ${checkNumber}, ${dateString(date)}, (${memo})")
                case DataModel.Other => None
            }))
        })

    val server = ZioHttpInterpreter().toHttp(transactionZEndpoint +: RestApi.swaggerServerEndpoints[Task])
}
