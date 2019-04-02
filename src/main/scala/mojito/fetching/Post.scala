package mojito.fetching

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._

final case class Post(content: String, users: List[UserId])

object Post extends Data[UserId, List[Post]] with FakeLatency with FakePostDB {
  def name = "Posts"

  def source[F[_] : ConcurrentEffect]: DataSource[F, UserId, List[Post]] = new DataSource[F, UserId, List[Post]] {
    def data = Post

    def CF = ConcurrentEffect[F]

    def fetch(id: UserId): F[Option[List[Post]]] =
      latency[F](s"One Post $id") >> CF.pure(getPostsForUser(id))

    override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, List[Post]]] =
      latency[F](s"Batch Posts $ids") >> CF.pure(ids.map(id => getPostsForUser(id).map(v => id -> v)).toList.flattenOption.toMap)

    private def getPostsForUser(id: UserId): Option[List[Post]] = {
      postDatabase.filter(_.users.contains_(id)) match {
        case Nil =>
          None
        case posts =>
          Some(posts)
      }
    }
  }
}

trait FakePostDB {
  val postDatabase: List[Post] = List(
    Post("content one", List()),
    Post("content two", List(2)),
    Post("content three", List(3)),
    Post("content four", List(1, 2, 3, 4))
  )
}
