package mojito.json

sealed trait Json extends Product with Serializable

object Json {

  final case class JsonObj(values: Map[String, Json]) extends Json

  final case class JsonArray(values: List[Json]) extends Json

  final case class JsonString(value: String) extends Json

  final case class JsonNumber(value: Double) extends Json

  final case class JsonBoolean(value: Boolean) extends Json

  final case object JsonNull extends Json

}
