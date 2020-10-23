package metacache.operations

import java.io.{File, FileInputStream, InputStream}

import cats.MonadError
import metacache.domain.ContentAndMetadata
import metacache.operations.ExtractContentMetadata.withStream
import metacache.util.CassandraUtils.from
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.{AutoDetectParser, ParseContext}
import org.apache.tika.sax.BodyContentHandler
import org.xml.sax.SAXException

class ExtractContentMetadata(context: ParseContext) {

  def apply[F[_]](filename: String)
                 (implicit me: MonadError[F, Throwable]): F[ContentAndMetadata] =
    withStream(filename, parseStream[F](filename))

  private def parseStream[F[_]](filename: String)
                               (stream: InputStream)
                               (implicit me: MonadError[F, Throwable]): F[ContentAndMetadata] = {

    val metadata = new Metadata()
    val handler = new BodyContentHandler()
    try {
      new AutoDetectParser().parse(stream, handler, metadata, context)
      me.pure(ContentAndMetadata(s"file://$filename", handler.toString, from(metadata)))
    } catch {
      case ex: SAXException => {
        println(ex.getClass.getSimpleName)
        me.pure(ContentAndMetadata(s"file://$filename", handler.toString, from(metadata)))
      }
      case ex: Exception => me.raiseError(ex)
    }
  }
}

object ExtractContentMetadata {

  def create = new ExtractContentMetadata(new ParseContext)

  private def withStream[F[_], R](filename: String,
                                  consumer: InputStream => F[R])
                                 (implicit me: MonadError[F, Throwable]): F[R] = {

    var stream: FileInputStream = null
    try {
      stream = new FileInputStream(new File(filename))
      consumer(stream)
    } catch {
      case e: Exception => me.raiseError(e)
    } finally {
      if(stream != null) {
        stream.close()
      }
    }
  }
}
