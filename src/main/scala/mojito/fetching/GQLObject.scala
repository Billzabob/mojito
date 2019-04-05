package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch.Fetch
import matryoshka.data.Fix
import mojito.schema.Fields._
import mojito.schema.Json.JsonObj
import mojito.schema.Tree.{Leaf, Node}
import mojito.schema.{Fields, Json, Tree}

object GQLObject {
  def getFieldsOfObject[F[_] : ConcurrentEffect, T : Fields](obj: T, tree: Tree[String]): Fetch[F, Fix[Json]] = tree match {
      case Node(_, subtrees) =>
        val fieldsMap = obj.fields
        subtrees.traverse { subTree =>
          val fieldName = subTree.value
          fieldsMap(fieldName).resolve(subTree).map(json => fieldName -> json)
        }.map(_.toMap).map(m => Fix(JsonObj(m)))
      case Leaf(value) =>
        Fetch.error(new Exception(s"Invalid request: $value"))
  }
}
