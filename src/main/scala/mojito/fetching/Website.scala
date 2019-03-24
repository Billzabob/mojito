package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._

final case class Website(siteTitle: String) {
  def user[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
    Fetch(id, User.source)

  def users[F[_] : ConcurrentEffect](ids: List[UserId]): Fetch[F, List[User]] =
    ids.traverse(user[F])

  def allUsers[F[_] : ConcurrentEffect]: Fetch[F, List[User]] =
    users(User.userDatabase.keys.toList)
}
