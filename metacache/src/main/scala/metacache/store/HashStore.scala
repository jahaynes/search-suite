package metacache.store

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.{BoundStatement, PreparedStatement}
import com.twitter.util.Future
import metacache.domain.DocFileHash
import metacache.util.FutureUtils.convert

class HashStore[F[_]](putImpl: DocFileHash => F[Unit],
                      getImpl: String => F[Option[DocFileHash]]) {

  def put(doc: DocFileHash): F[Unit] = putImpl(doc)

  def get(docUri: String): F[Option[DocFileHash]] = getImpl(docUri)
}

object HashStore {

  private val cqlTable =
    """
      | CREATE TABLE IF NOT EXISTS metacache.hash
      | (doc_uri text PRIMARY KEY, file_hash int);
    """.stripMargin

  private val cqlAddHash =
    """
      | INSERT INTO metacache.hash (doc_uri, file_hash)
      | VALUES (?, ?)
      | IF NOT EXISTS;
    """.stripMargin

  private val cqlQuery =
    """
      | SELECT doc_uri, file_hash
      | FROM metacache.hash
      | WHERE doc_uri = ?;
    """.stripMargin

  def create(documentStore: DocumentStore): Future[HashStore[Future]] =
    documentStore match {
      case DocumentStore(session) =>
        for {
          _       <- convert(session.executeAsync(cqlTable))
          stmtAdd <- convert(session.prepareAsync(cqlAddHash))
          stmtQry <- convert(session.prepareAsync(cqlQuery))
        } yield new HashStore[Future](addHash(session, stmtAdd),
                                      queryDoc(session, stmtQry))
    }

  private def addHash(session: CqlSession,
                      stmt: PreparedStatement)
                      (doc: DocFileHash): Future[Unit] = {
    val bound: BoundStatement = doc match {
      case DocFileHash(a, b) => stmt.bind(s"file://$a",b)
    }
    convert(session.executeAsync(bound)) map {_ => ()}
  }

  private def queryDoc(session: CqlSession,
                       stmt: PreparedStatement)
                      (docUri: String): Future[Option[DocFileHash]] = {
    import metacache.util.CassandraUtils.getOne

    val bound = stmt.bind(s"file://$docUri")
    convert(session.executeAsync(bound)) map {
      getOne
    } map {
      case None => None
      case Some(row) =>
        Some(DocFileHash(
          row.getString("doc_uri"),
          row.getInt("file_hash")
        ))
    }
  }
}
