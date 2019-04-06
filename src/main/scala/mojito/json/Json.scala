package mojito.json

sealed trait Json[T] extends Product with Serializable

object Json {

  // TODO: Use matryoshka here. Should help with figuring out he other stuff.
  final case class JsonObj[T](values: Map[String, T]) extends Json[T]

  final case class JsonArray[T](values: List[T]) extends Json[T]

  final case class JsonString[T](value: String) extends Json[T]

  final case class JsonNumber[T](value: Double) extends Json[T]

  final case class JsonBoolean[T](value: Boolean) extends Json[T]

  // TODO: This seems weird, but case object can't take a type parameter.
  final case class JsonNull[T]() extends Json[T]

}
