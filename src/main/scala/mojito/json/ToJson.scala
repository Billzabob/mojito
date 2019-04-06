package mojito.json

import cats.Foldable
import matryoshka.data.Fix
import mojito.json.Json._

trait ToJson[T] {
  def toJson(t: T): Fix[Json]
}

object ToJson {

  def apply[T](implicit instance: ToJson[T]): ToJson[T] = instance

  def instance[T](func: T => Fix[Json]): ToJson[T] = new ToJson[T] {
    override def toJson(t: T) = func(t)
  }

  implicit class ToJsonOps[T](t: T) {
    def toJson(implicit ev: ToJson[T]): Fix[Json] = ev.toJson(t)
  }

  implicit val fromString: ToJson[String] = instance(s => Fix(JsonString(s)))

  implicit val fromInt: ToJson[Int] = instance(i => Fix(JsonNumber(i.toDouble)))

  implicit val fromLong: ToJson[Long] = instance(l => Fix(JsonNumber(l.toDouble)))

  implicit val fromFloat: ToJson[Float] = instance(f => Fix(JsonNumber(f.toDouble)))

  implicit val fromDouble: ToJson[Double] = instance(d => Fix(JsonNumber(d)))

  implicit val fromBoolean: ToJson[Boolean] = instance(b => Fix(JsonBoolean(b)))

  implicit def fromOption[T](implicit ev: ToJson[T]): ToJson[Option[T]] = instance(_.fold[Fix[Json]](Fix(JsonNull()))(ev.toJson))

  implicit def fromFoldable[F[_], T](implicit ev: ToJson[T], ev2: Foldable[F]): ToJson[F[T]] = instance(ft => Fix(JsonArray(ev2.toList(ft).map(ev.toJson))))

  implicit def fromMap[T](implicit ev: ToJson[T]): ToJson[Map[String, T]] = instance(map => Fix(JsonObj(map.mapValues(ev.toJson))))
}
