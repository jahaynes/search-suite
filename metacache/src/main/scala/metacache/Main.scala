package metacache

import java.net.InetSocketAddress.createUnresolved
import com.twitter.util.{Await, Future}
import io.catbird.util.twitterFutureInstance
import metacache.operations.ExtractContentMetadata
import metacache.store.{ContentAndMetadataStore, DocumentStore, HashStore}
import metacache.util.ExtractController

object Main {

  def main(args: Array[String]): Unit = {

    args match {

      case Array(host, strPort) => {

        val port = Integer.parseInt(strPort)
        val cassandraAddress = createUnresolved(host, port)

        val extractor = ExtractContentMetadata.create

        val service =
          for {
            docStore         <- DocumentStore.create(cassandraAddress)
            contentMetaStore <- ContentAndMetadataStore.create(docStore)
            hashStore        <- HashStore.create(docStore)
          } yield MetacacheController.run(8080, extractor.apply[Future], contentMetaStore, hashStore)

        Await.result(service)

      };

      case Array() => {
        val extractor = ExtractContentMetadata.create
        val service = ExtractController.run(8080, extractor.apply[Future])
        Await.result(service)
      }

      case _ => throw new RuntimeException("Usage: [host, port] or [] for no cassandra")
    }
  }
}
