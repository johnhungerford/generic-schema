import org.hungerford.generic.schema.example.api.{DataModel, RestApi, RestServer}
import zhttp.http.*
import zhttp.service.Server
import zio.*

import java.util.UUID

object Main extends ZIOAppDefault {
    override def run: ZIO[ Any, Any, Any ] =
        Console.printLine(RestApi.docsString) *> Server.start(8090, RestServer.server)
}
