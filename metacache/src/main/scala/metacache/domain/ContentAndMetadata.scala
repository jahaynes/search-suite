package metacache.domain

import java.util.{Map => JMap}

case class ContentAndMetadata(docUri:   String,
                              content:  String,
                              metadata: JMap[String,String])
