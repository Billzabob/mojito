package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._

final case class Website(siteTitle: String, users: List[UserId]) {
  def allUsers[F[_] : ConcurrentEffect]: Fetch[F, List[User]] =
    users.traverse(user[F])

  private def user[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
    Fetch(id, User.source)
}
