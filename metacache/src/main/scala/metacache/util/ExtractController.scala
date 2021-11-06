package metacache.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Http, ListeningServer, Service}
import com.twitter.util.Future
import metacache.Filepath
import metacache.domain.ContentAndMetadata

class ExtractController(requestParser: String => Future[Filepath],
                        extractImpl: String => Future[ContentAndMetadata])
  extends Service[Request, Response] {

  import com.twitter.finagle.http.Method.Post
  import com.twitter.finagle.http.path.{->, /, Path, Root}
  import metacache.util.ExtractController.{notFound, ok}

  def apply(req: Request): Future[Response] =
    req.method -> Path(req.path) match {
      case Post -> Root / "extract" => extract(req)
      case _ => Future(notFound(req))
    }

  private def extract(req: Request): Future[Response] =
    for {
      filePath <- requestParser(req.contentString)
      contentMetadata <- extractImpl(filePath.value)
    } yield ok(contentMetadata.content)
}

object ExtractController {

  private def ok(body: String): Response = {
    val response = Response(Status.Ok)
    response.setContentString(body)
    response
  }

  private def notFound(req: Request): Response = {
    val response = Response(Status.NotFound)
    response.setContentString(s"Route not found: ${req.path}")
    response
  }

  private def parseReq(objectMapper: ObjectMapper)
                      (json: String): Future[Filepath] =
    try {
      Future(objectMapper.readValue(json, classOf[Filepath]))
    } catch {
      case ex: Exception => Future.exception(ex)
    }

  def run(port: Int,
          extractMetadata: String => Future[ContentAndMetadata]): ListeningServer = {
    val objectMapper = new ObjectMapper
    objectMapper.registerModule(DefaultScalaModule)
    Http.serve(addr = s":$port", new ExtractController(parseReq(objectMapper), extractMetadata))
  }
}