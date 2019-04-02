package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._
import mojito.fetching.GQLObject.NameMeToo
import mojito.schema.JsonType.JsonArray

final case class Website(siteTitle: String, users: List[UserId]) extends GQLObject {
  def fields[F[_] : ConcurrentEffect]: Map[String, NameMeToo[F]] = List(
    "allUsers" -> {
      // TODO: Add method that does this for us and uses ToJsonType
      val foo: NameMeToo[F] = tree => allUsers.flatMap(_.traverse(_.nameMe(tree)).map(JsonArray))
      foo
    }
  ).toMap

  def allUsers[F[_] : ConcurrentEffect]: Fetch[F, List[User]] =
    users.traverse(user(_))

  def user[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
    Fetch(id, User.source)
}
