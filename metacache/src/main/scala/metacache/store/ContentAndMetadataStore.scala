package metacache.store

import metacache.domain.ContentAndMetadata

class ContentAndMetadataStore[F[_]](putImpl: ContentAndMetadata => F[Unit],
                                    getImpl: String => F[Option[ContentAndMetadata]]) {

  def put(doc: ContentAndMetadata): F[Unit] = putImpl(doc)

  def get(docUri: String): F[Option[ContentAndMetadata]] = getImpl(docUri)
}

object ContentAndMetadataStore {

  import com.datastax.oss.driver.api.core.cql.{BoundStatement, PreparedStatement}
  import com.datastax.oss.driver.api.core.CqlSession
  import com.twitter.util.Future
  import metacache.util.FutureUtils._

  def create(documentStore: DocumentStore): Future[ContentAndMetadataStore[Future]] =
    documentStore match {
      case DocumentStore(session) =>
        for {
          _       <- convert(session.executeAsync(cqlTable))
          stmtAdd <- convert(session.prepareAsync(cqlAddDoc))
          stmtQry <- convert(session.prepareAsync(cqlQuery))
        } yield new ContentAndMetadataStore[Future](
          putImpl = addDoc(session, stmtAdd),
          getImpl = queryDoc(session, stmtQry)
        )
    }

  private def addDoc(session: CqlSession,
                     stmt: PreparedStatement)
                     (doc: ContentAndMetadata): Future[Unit] = {
    val bound: BoundStatement = doc match {
      case ContentAndMetadata(a, b, c) => stmt.bind(a,b,c)
    }
    convert(session.executeAsync(bound)) map {_ => ()}
  }

  private def queryDoc(session: CqlSession,
                       stmt: PreparedStatement)
                      (docUri: String): Future[Option[ContentAndMetadata]] = {
    import metacache.util.CassandraUtils.getOne

    val bound = stmt.bind(s"file://$docUri")
    convert(session.executeAsync(bound)) map {
      getOne(_)
    } map {
      case None => None
      case Some(row) =>
      Some(ContentAndMetadata(
        row.getString("doc_uri"),
        row.getString("content"),
        row.getMap("metadata", classOf[String], classOf[String])
      ))
    }
  }

  private val cqlTable =
    """
      | CREATE TABLE IF NOT EXISTS metacache.content_metadata
      | (doc_uri text PRIMARY KEY, content text, metadata map<text,text>);
    """.stripMargin

  private val cqlAddDoc =
    """
      | INSERT INTO metacache.content_metadata (doc_uri, content, metadata)
      | VALUES (?, ?, ?)
      | IF NOT EXISTS;
    """.stripMargin

  private val cqlQuery =
    """
      | SELECT doc_uri, content, metadata
      | FROM metacache.content_metadata
      | WHERE doc_uri = ?;
    """.stripMargin
}
