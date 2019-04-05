package mojito

import cats.Foldable
import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp}
import fetch.Fetch
import matryoshka.data.Fix
import mojito.fetching.{GQLObject, Website}
import mojito.schema.Json._
import mojito.schema.Tree.{Leaf, Node}

object schema {

  sealed trait Json[T] extends Product with Serializable

  object Json {

    // TODO: Use matryoshka here. Should help with figuring out he other stuff.
    final case class JsonObj[T](values: Map[String, T]) extends Json[T]

    final case class JsonArray[T](values: List[T]) extends Json[T]

    final case class JsonString[T](value: String) extends Json[T]

    final case class JsonNumber[T](value: Double) extends Json[T]

    final case class JsonBoolean[T](value: Boolean) extends Json[T]

    // TODO: This seems weird, but case object can't take a type parameter. Doing:
    //  final case object JsonNull extends Json[Nothing]
    //  only works if Json is covariant in T, which might work?
    final case class JsonNull[T]() extends Json[T]
  }

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

  final case class Field[F[_]](resolve: Tree[String] => Fetch[F, Fix[Json]])

  trait Fields[T] {
    def fields[F[_] : ConcurrentEffect](t: T): Map[String, Field[F]]
  }

  object Fields {
    def apply[T](implicit instance: Fields[T]): Fields[T] = instance

    implicit class FieldsOps[T](t: T) {
      def fields[F[_] : ConcurrentEffect](implicit ev: Fields[T]): Map[String, Field[F]] = ev.fields(t)
    }

  }

  sealed trait Tree[T] extends Product with Serializable {
    val value: T
  }

  object Tree {

    final case class Node[T](value: T, subTrees: List[Tree[T]]) extends Tree[T]

    final case class Leaf[T](value: T) extends Tree[T]

  }

}

object SchemaExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

    val website = Website("Title", List(1, 2, 3, 4))

    val tree = Node("query", List(Node("allUsers", List(Leaf("id")))))

    def stuff[F[_] : ConcurrentEffect] = GQLObject.getFieldsOfObject(website, tree)

    for {
      _ <- IO(println("Starting"))
      results <- Fetch.run[IO](stuff)
      _ <- IO(println(s"Results: $results"))
    } yield ExitCode.Success
  }
}
