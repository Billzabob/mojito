package mojito

import cats.effect.ConcurrentEffect
import fetch.Fetch
import mojito.fetching.{Post, User, Website}

object schema {

  trait HasId[T] {
    type Id
    def Id(t: T): Id
  }

  object HasId {
    type Aux[T, S] = HasId[T] { type Id = S }

    implicit def apply[T](implicit ev: HasId[T]): ev.type = ev
  }

  sealed trait GQLType extends Product with Serializable

  object GQLType {
    final case class GQLObj(values: Map[String, GQLType]) extends GQLType
    final case class GQLArray(values: List[GQLType]) extends GQLType
    final case class GQLString(value: String) extends GQLType
    final case class GQLNumber(value: Double) extends GQLType
  }

  final case class Field[F[_], T](resolve: T => Fetch[F, GQLType], description: Option[String])

  trait Fields[F[_], T] {
    val fields: Map[String, Field[F, T]]
  }

  def usersForWebsite[F[_] : ConcurrentEffect](website: Website): Fetch[F, List[User]] =
    website.allUsers[F]

  def postsForUser[F[_] : ConcurrentEffect](user: User): Fetch[F, List[Post]] =
    user.allPosts[F]

  implicit def websiteFields[F[_] : ConcurrentEffect]: Fields[F, Website] = new Fields[F, Website] {
    val fields: Map[String, Field[F, Website]] = List(
      "users" -> Field((_: Website) => ??? : Fetch[F, GQLType], None)
    ).toMap
  }

  implicit def userFields[F[_] : ConcurrentEffect]: Fields[F, User] = new Fields[F, User] {
    val fields: Map[String, Field[F, User]] = List(
      "posts" -> Field((_: User) => ??? : Fetch[F, GQLType], None)
    ).toMap
  }
}
