package mojito

import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp}
import fetch.Fetch
import matryoshka.data.Fix
import mojito.fetching.{GQLObject, Website}
import mojito.json.Json
import mojito.schema.Tree.{Leaf, Node}

object schema {

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
