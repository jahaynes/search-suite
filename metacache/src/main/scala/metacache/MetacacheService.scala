package metacache

import cats.MonadError
import cats.syntax.functor._, cats.syntax.flatMap._
import com.twitter.finagle.http.{Request, Response, Status}
import metacache.domain.{ContentAndMetadata, DocFileHash}
import metacache.operations.FileHash
import metacache.store.{ContentAndMetadataStore, HashStore}

case class Filepath(value: String)

class MetacacheService[F[_]](parser:                  String => F[Filepath],
                             extractMetadata:         String => F[ContentAndMetadata],
                             contentAndMetadataStore: ContentAndMetadataStore[F],
                             hashStore:               HashStore[F])
                             (implicit me: MonadError[F, Throwable]) {

  import MetacacheService._

  def checkFile(req: Request) =
    for {
      filePath        <- parser(req.contentString)
      contentMetadata <- contentAndMetadataStore.get(filePath.value)
      fileHash        <- hashStore.get(filePath.value)
    } yield render (contentMetadata, fileHash)

  def ingestOrGet(req: Request) = {
        val job = {
          val fContentMetadata =
            for {
              filePath        <- parser(req.contentString) // TODO .raiseError(Duration.fromSeconds(2))
              contentMetadata <- contentAndMetadataStore.get(filePath.value)
            } yield contentMetadata
          fContentMetadata flatMap {
            contentMetadata: Option[ContentAndMetadata] => {
              if (contentMetadata.isDefined) {
                me.pure(contentMetadata.get.content)
              } else {
                for {
                  filePath        <- parser(req.contentString) map { _.value }
                  contentMetadata <- extractMetadata(filePath)
                  _               <- contentAndMetadataStore.put(contentMetadata)
                  fileHash        <- FileHash(filePath)
                  _               <- hashStore.put(fileHash)
                } yield contentMetadata.content
              }
            }
          }
        }
        renderJob(req, job)
      }

  def ingestFile(req: Request) = {
        val job = {
          val fPresent = for {
            filePath        <- parser(req.contentString)
            contentMetadata <- contentAndMetadataStore.get(filePath.value)
          } yield contentMetadata.isDefined
          fPresent flatMap {
            case true => me.pure("already done")
            case false =>
              for {
                filePath        <- parser(req.contentString) map {_.value}
                contentMetadata <- extractMetadata(filePath)
                _               <- contentAndMetadataStore.put(contentMetadata)
                fileHash        <- FileHash(filePath)
                _               <- hashStore.put(fileHash)
              } yield contentMetadata.content
          }
        }
        renderJob(req, job)
      }
}

object MetacacheService {

  import com.fasterxml.jackson.databind.ObjectMapper
  import com.fasterxml.jackson.module.scala.DefaultScalaModule

  def renderJob[F[_]](req: Request,
                      job: F[String])
                      (implicit me: MonadError[F, Throwable]): F[Response] = 
    me.handleErrorWith {
      job map {
        content => println(s"OK:\t${req.contentString}")
                   val resp = Response(Status.Ok)
                   resp.setContentString(content)
                   resp
      }
    } {
      ex => println(s"BAD:\t${req.contentString} ${ex.getMessage}")
            val response = Response(Status.NotAcceptable)
            response.setContentString(ex.getMessage)
            me.pure(response)
    }

  def render(inputs: (Option[ContentAndMetadata],
                      Option[DocFileHash])): Response =
    inputs match {
      case (ocm, odf) => {
        val resp = Response(Status.Ok)
        val m = Map("content"  -> ocm.map(cmd => cmd.content).orNull,
                    "metadata" -> ocm.map(cmd => cmd.metadata).orNull,
                    "filehash" -> odf.map(df => df.fileHash).orNull)
        resp.setContentString(m.filter(kv => kv._2 != null).toString)
        resp
      }
    }
}
