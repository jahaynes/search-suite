package metacache.operations

import java.io.FileInputStream

import cats.MonadError
import metacache.domain.DocFileHash
import net.jpountz.xxhash.{StreamingXXHash32, XXHashFactory}

import scala.annotation.tailrec

object FileHash {

  def apply[F[_]](filePath: String)
                 (implicit me: MonadError[F, Throwable]): F[DocFileHash] = {
    val factory = XXHashFactory.fastestInstance
    val buf = new Array[Byte](8192)
    var in: FileInputStream = null
    try {
      me.pure {
        in = new FileInputStream(filePath)
        @tailrec
        def loop(hash32: StreamingXXHash32): Int =
          in.read(buf) match {
            case -1 => hash32.getValue
            case read =>
              hash32.update(buf, 0, read)
              loop(hash32)
          }
        DocFileHash(filePath, loop(factory.newStreamingHash32(0xdeadbeef)))
      }
    } catch {
      case ex: Exception => me.raiseError(ex)
    } finally {
      if (in != null) {
        in.close()
      }
    }
  }
}
