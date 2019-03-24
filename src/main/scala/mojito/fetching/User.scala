package mojito.fetching

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._

final case class User(id: UserId, username: String, posts: List[PostId]) {
  def allPosts[F[_] : ConcurrentEffect]: Fetch[F, List[Post]] =
    posts.traverse(post[F])

  private def post[F[_] : ConcurrentEffect](id: PostId) =
    Fetch(id, Post.source)
}

object User extends Data[UserId, User] with FakeLatency with FakeUserDB {
  def name = "Users"

  def source[F[_] : ConcurrentEffect]: DataSource[F, UserId, User] = new DataSource[F, UserId, User] {
    override def data = User

    override def CF = ConcurrentEffect[F]

    override def fetch(id: UserId): F[Option[User]] =
      latency[F](s"One User $id") >> CF.pure(userDatabase.get(id))

    override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
      latency[F](s"Batch Users $ids") >> CF.pure(userDatabase.filterKeys(ids.toList.toSet))
  }
}

trait FakeUserDB {
  val userDatabase: Map[UserId, User] = Map(
    1 -> User(1, "@one", List(1, 2)),
    2 -> User(2, "@two", List(2, 3)),
    3 -> User(3, "@three", List(3, 4)),
    4 -> User(4, "@four", List())
  )
}
