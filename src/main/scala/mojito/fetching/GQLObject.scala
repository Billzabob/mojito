package mojito.fetching

import cats.effect.ConcurrentEffect
import fetch.Fetch
import mojito.fetching.GQLObject.NameMeToo
import mojito.schema.JsonType.JsonObj
import mojito.schema.Tree.{Leaf, Node}
import mojito.schema.{JsonType, Tree}
import cats.implicits._

// TODO: Use Fields typeclass instead
trait GQLObject {
  def nameMe[F[_] : ConcurrentEffect](tree: Tree[String]): Fetch[F, JsonType] = {
    tree match {
      case Node(_, subtrees) =>
        subtrees.traverse { subTree =>
          val field = subTree.value
          fields.getOrElse(field, GQLObject.noSuchFieldError(field))(subTree).map(json => field -> json)
        }.map(_.toMap).map(JsonObj)
      case Leaf(value) =>
        Fetch.error(new Exception(s"Invalid request: $value"))
    }
  }

  def fields[F[_] : ConcurrentEffect]: Map[String, NameMeToo[F]]
}

object GQLObject {
  def noSuchFieldError[F[_] : ConcurrentEffect](field: String): NameMeToo[F] = _ => Fetch.error[F, JsonType](new Exception(s"No such field: $field"))

  type NameMeToo[F[_]] = Tree[String] => Fetch[F, JsonType]
}
