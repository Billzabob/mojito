package mojito

import cats.Foldable
import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp}
import fetch.Fetch
import mojito.fetching.Website
import mojito.schema.JsonType.{JsonArray, JsonBoolean, JsonNull, JsonNumber, JsonString}
import mojito.schema.Tree.{Leaf, Node}

object schema {

  sealed trait JsonType extends Product with Serializable

  object JsonType {

    final case class JsonObj(values: Map[String, JsonType]) extends JsonType

    final case class JsonArray(values: List[JsonType]) extends JsonType

    final case class JsonString(value: String) extends JsonType

    final case class JsonNumber(value: Double) extends JsonType

    final case class JsonBoolean(value: Boolean) extends JsonType

    final case object JsonNull extends JsonType

  }

  trait ToJsonType[T] {
    def toJsonType(t: T): JsonType
  }

  object ToJsonType {

    def apply[T](implicit instance: ToJsonType[T]): ToJsonType[T] = instance

    def instance[T](func: T => JsonType): ToJsonType[T] = new ToJsonType[T] {
      override def toJsonType(t: T) = func(t)
    }

    implicit val fromString: ToJsonType[String] = instance(JsonString)

    implicit val fromInt: ToJsonType[Int] = instance(i => JsonNumber(i.toDouble))

    implicit val fromLong: ToJsonType[Long] = instance(l => JsonNumber(l.toDouble))

    implicit val fromFloat: ToJsonType[Float] = instance(f => JsonNumber(f.toDouble))

    implicit val fromDouble: ToJsonType[Double] = instance(JsonNumber)

    implicit val fromBoolean: ToJsonType[Boolean] = instance(JsonBoolean)

    implicit def fromOption[T](implicit ev: ToJsonType[T]): ToJsonType[Option[T]] = instance(_.fold[JsonType](JsonNull)(ev.toJsonType))

    implicit def fromFoldable[F[_], T](implicit ev: ToJsonType[T], ev2: Foldable[F]): ToJsonType[F[T]] = instance(ft => JsonArray(ev2.toList(ft).map(ev.toJsonType)))
  }

  final case class Field[F[_]](resolve: Tree[String] => Fetch[F, JsonType], description: Option[String])

  // TODO: Use this type class instead of GQLObject
  trait Fields[F[_], T] {
    val fields: Map[String, Field[F]]
  }

  sealed trait Tree[T] extends Product with Serializable {
    val value: T
  }

  object Tree {

    final case class Node[T](value: T, subTrees: List[Tree[T]]) extends Tree[T]

    final case class Leaf[T](value: T) extends Tree[T]

  }

//  implicit def websiteFields[F[_] : ConcurrentEffect]: Fields[F, Website] = new Fields[F, Website] {
//    val fields: Map[String, Field[F, Website]] = List(
//      "users" -> Field((_: Website) => ??? : Fetch[F, GQLType], None)
//    ).toMap
//  }
//
//  implicit def userFields[F[_] : ConcurrentEffect]: Fields[F, User] = new Fields[F, User] {
//    val fields: Map[String, Field[F, User]] = List(
//      "posts" -> Field((_: User) => ??? : Fetch[F, GQLType], None)
//    ).toMap
//  }
}

object SchemaExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

    val website = Website("Title", List(1, 2, 3, 4))

    val tree = Node("query", List(Node("allUsers", List(Leaf("id")))))

    def stuff[F[_] : ConcurrentEffect] = website.nameMe(tree)

    for {
      _ <- IO(println("Starting"))
      results <- Fetch.run[IO](stuff)
      _ <- IO(println(s"Results: $results"))
    } yield ExitCode.Success
  }
}
