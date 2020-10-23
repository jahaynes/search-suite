package metacache

import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Http, ListeningServer, Service}
import com.twitter.util.{Duration, Future}
import metacache.domain.{ContentAndMetadata, DocFileHash}
import metacache.operations.FileHash
import metacache.store.{ContentAndMetadataStore, HashStore}

class MetacacheController(metacacheService: MetacacheService[Future])
        extends Service[Request, Response] {

  import com.twitter.finagle.http.Method.Post
  import com.twitter.finagle.http.path.{->, /, Path, Root}
  import io.catbird.util.twitterFutureInstance
  import metacache.operations.ExtractContentMetadata

  import com.twitter.finagle.util.DefaultTimer.Implicit

  def apply(req: Request): Future[Response] =
    req.method -> Path(req.path) match {
      case Post -> Root / "checkFile"   => metacacheService.checkFile(req)
      case Post -> Root / "ingestOrGet" => metacacheService.ingestOrGet(req)
      case Post -> Root / "ingestFile"  => metacacheService.ingestFile(req)
      case _                            => Future(notFound)
    }

  private def notFound: Response = {
    val response = Response(Status.NotFound)
    response.setContentString("Route not found")
    response
  }
}

object MetacacheController {

  import io.catbird.util.twitterFutureInstance
  import com.fasterxml.jackson.databind.ObjectMapper
  import com.fasterxml.jackson.module.scala.DefaultScalaModule

  def run(port:                    Int,
          extractMetadata:         String => Future[ContentAndMetadata],
          contentAndMetadataStore: ContentAndMetadataStore[Future],
          hashStore:               HashStore[Future]): ListeningServer = {

    val objectMapper = new ObjectMapper
    objectMapper.registerModule(DefaultScalaModule)

    val metacacheService =
        new MetacacheService[Future](parseReq(objectMapper),
                                     extractMetadata,
                                     contentAndMetadataStore,
                                     hashStore)

    val metacacheController =
        new MetacacheController(metacacheService)

    Http.serve(addr = s":$port", metacacheController)
  }

  def parseReq(objectMapper: ObjectMapper)
              (json: String): Future[Filepath] =
    try {
      Future(objectMapper.readValue(json, classOf[Filepath]))
    } catch {
      case ex: Exception => Future.exception(ex)
    }
}