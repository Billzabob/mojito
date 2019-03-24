package mojito.fetching

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._

final case class Post(id: PostId, content: String)

object Post extends Data[PostId, Post] with FakeLatency with FakePostDB {
  def name = "Posts"

  def source[F[_] : ConcurrentEffect]: DataSource[F, PostId, Post] = new DataSource[F, PostId, Post] {
    override def data = Post

    override def CF = ConcurrentEffect[F]

    override def fetch(id: PostId): F[Option[Post]] =
      latency[F](s"One User $id") >> CF.pure(postDatabase.get(id))

    override def batch(ids: NonEmptyList[PostId]): F[Map[PostId, Post]] =
      latency[F](s"Batch Users $ids") >> CF.pure(postDatabase.filterKeys(ids.toList.toSet))
  }
}

trait FakePostDB {
  val postDatabase: Map[PostId, Post] = Map(
    "1" -> Post("1", "@one"),
    "2" -> Post("2", "@two"),
    "3" -> Post("3", "@three"),
    "4" -> Post("4", "@four")
  )
}
