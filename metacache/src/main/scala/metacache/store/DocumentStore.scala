package metacache.store

import java.net.InetSocketAddress

import com.datastax.oss.driver.api.core.CqlSession
import com.twitter.util.Future
import metacache.util.FutureUtils.convert

case class DocumentStore(session: CqlSession)

object DocumentStore {

  private val cqlKeyspace =
    """
      | CREATE KEYSPACE IF NOT EXISTS metacache
      | WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 2}
      | AND DURABLE_WRITES = true
    """.stripMargin

  def create(cassandraAddress: InetSocketAddress): Future[DocumentStore] = {

    val asyncSession =
      CqlSession.builder
                .addContactPoint(cassandraAddress)
                .withLocalDatacenter("datacenter1") //TODO why 'datacenter1' ?
                .buildAsync()

    for {
      session <- convert(asyncSession)
      _       <- convert(session.executeAsync(cqlKeyspace))
    } yield new DocumentStore(session)
  }
}
