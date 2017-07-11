package boggle

import java.io.File

import akka.actor.ActorSystem
import colossus._
import colossus.core._
import colossus.protocols.http.HttpMethod._
import colossus.protocols.http.UrlParsing._
import colossus.protocols.http._
import colossus.service.Callback

import scala.io.Source
import scala.util.parsing.json._
import scala.util.{Failure, Success, Try}

class RouterService(context: ServerContext)(implicit boggleService: BoggleService) extends HttpService(context) {

  def handle: PartialFunction[HttpRequest, Callback[HttpResponse]] = {
    case request @ Post on Root / "solve" =>
      val body = request.body.as[String]

      body match {
        case Success(content) =>
          val solution = Try {
            val parsed = JSON.parseFull(content).getOrElse(throw new Exception(s"Cannot parse POST body: $content"))
            val board = parsed.asInstanceOf[List[List[String]]]

            boggleService.solve(board)
          }
          solution match {
            case Success(obj) => Callback.successful(request.ok(HttpBody(JSONArray(obj.toList).toString(),
              "application/json")))
            case Failure(e) => Callback.successful(request.error(e.getMessage))
          }
        case Failure(e) =>
          Callback.successful(request.error(s"Cannot parse POST body with error: ${e.getMessage}"))
      }
    case request @ Get on Root =>
      val projectRoot = new File(".").getCanonicalPath
      val source = Source.fromFile(s"$projectRoot/src/main/ui/index.html")
      val lines = source.getLines().mkString("\n")
      val body = HttpBody(lines, "text/html")

      Callback.successful(request.ok(body))
  }

}

class RouterInitializer(worker: WorkerRef)(implicit boggleService: BoggleService) extends Initializer(worker) {

  def onConnect = context => new RouterService(context)

}

object Main extends App {

  implicit val actorSystem = ActorSystem()
  implicit val io = IOSystem()
  implicit val boggleService = new BoggleService()

  Server.start("boggle", 9000) { worker => new RouterInitializer(worker) }

}
