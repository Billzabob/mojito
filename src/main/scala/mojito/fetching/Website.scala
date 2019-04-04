package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._
import mojito.schema.Json.JsonArray
import mojito.schema.{Field, Fields}

final case class Website(siteTitle: String, users: List[UserId]) {

  def allUsers[F[_] : ConcurrentEffect]: Fetch[F, List[User]] =
    users.traverse(user(_))

  def user[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
    Fetch(id, User.source)
}

object Website {
  implicit def websiteFields: Fields[Website] = new Fields[Website] {
    def fields[F[_] : ConcurrentEffect](website: Website): Map[String, Field[F]] = List(
      "allUsers" -> Field(tree => website.allUsers.flatMap(_.traverse(a => GQLObject.getFieldsOfObject(a, tree)).map(JsonArray)))
    ).toMap
  }
}
