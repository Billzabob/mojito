package mojito.fetching

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.implicits._
import fetch._
import mojito.schema.Json.JsonNumber
import mojito.schema.{Field, Fields}

final case class User(id: UserId, username: String) {

  def allPosts[F[_] : ConcurrentEffect]: Fetch[F, List[Post]] =
    Fetch(id, Post.source)
}

object User extends Data[UserId, User] with FakeLatency with FakeUserDB {
  def name = "Users"

  def source[F[_] : ConcurrentEffect]: DataSource[F, UserId, User] = new DataSource[F, UserId, User] {
    def data = User

    def CF = ConcurrentEffect[F]

    def fetch(id: UserId): F[Option[User]] =
      latency[F](s"One User $id") >> CF.pure(userDatabase.get(id))

    override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
      latency[F](s"Batch Users $ids") >> CF.pure(userDatabase.filterKeys(ids.toList.toSet))
  }

  implicit def userFields: Fields[User] = new Fields[User] {
    def fields[F[_] : ConcurrentEffect](user: User): Map[String, Field[F]] = List(
      "id" -> Field(_ => Fetch.pure(JsonNumber(user.id.toDouble)))
    ).toMap
  }
}

trait FakeUserDB {
  val userDatabase: Map[UserId, User] = Map(
    1 -> User(1, "@one"),
    2 -> User(2, "@two"),
    3 -> User(3, "@three"),
    4 -> User(4, "@four")
  )
}
