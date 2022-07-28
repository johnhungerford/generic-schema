import org.hungerford.generic.schema.example.api.{RestApi, RestServer}
import zhttp.service.Server

import java.util.UUID

import zio.*
import zhttp.http._
import zhttp.service.Server

object Main extends ZIOAppDefault {

    override def run: ZIO[ Any, Any, Any ] =
        Console.printLine(RestApi.docsString) *> Server.start(8090, RestServer.server)

}
