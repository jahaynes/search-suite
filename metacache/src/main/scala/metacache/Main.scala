package metacache

import java.net.InetSocketAddress.createUnresolved

import com.twitter.util.{Await, Future}
import io.catbird.util.twitterFutureInstance
import metacache.operations.ExtractContentMetadata
import metacache.store.{ContentAndMetadataStore, DocumentStore, HashStore}

object Main {

    def main(args: Array[String]): Unit = {

        if (args.length != 2) {
            throw new RuntimeException("Usage run host port")
        }

        val host = args(0)
        val port = Integer.parseInt(args(1))
        val cassandraAddress = createUnresolved(host, port)

        val extractor = ExtractContentMetadata.create

        val service =
          for {
              docStore         <- DocumentStore.create(cassandraAddress)
              contentMetaStore <- ContentAndMetadataStore.create(docStore)
              hashStore        <- HashStore.create(docStore)
          } yield MetacacheController.run(8080, extractor.apply[Future], contentMetaStore, hashStore)

        Await.result(service)
    }
}
