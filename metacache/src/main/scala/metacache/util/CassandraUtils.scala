package metacache.util

import java.util
import java.util.{Map => JMap}

import com.datastax.oss.driver.api.core.cql.{AsyncResultSet, Row}
import org.apache.tika.metadata.Metadata

object CassandraUtils {

  def getOne(rs: AsyncResultSet): Option[Row] = {
    val it = rs.currentPage().iterator()  //TODO is currentPage principled?
    if (it.hasNext) {
      Some(it.next)
    } else {
      None
    }
  }

  def from(metadata: Metadata): JMap[String, String] = {
    val hashMap = new util.HashMap[String, String]
    metadata.names foreach {
      name => hashMap.put(name, metadata.get(name))
    }
    hashMap
  }
}
