package metacache.util

import io.circe.parser.decode
import io.circe.{Decoder, Encoder, Error}

object Serialisation {
  //def toJson[A](a: A)(implicit enc: Encoder[A]): String = a.asJson.spaces2
  //def fromJson[A](json: String)(implicit dec: Decoder[A]): Either[Error, A] = decode[A](json)
}
