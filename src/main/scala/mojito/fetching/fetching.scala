package mojito.fetching

import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import fetch._

object fetching extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    type UserId = Int
    type PostId = String
    final case class User(id: UserId, username: String)
    final case class Post(id: PostId, content: String)

    def latency[F[_] : ConcurrentEffect](msg: String): F[Unit] = for {
      _ <- Sync[F].delay(println(s"--> [${Thread.currentThread.getId}] $msg"))
      _ <- Sync[F].delay(Thread.sleep(100))
      _ <- Sync[F].delay(println(s"<-- [${Thread.currentThread.getId}] $msg"))
    } yield ()

    val userDatabase: Map[UserId, User] = Map(
      1 -> User(1, "@one"),
      2 -> User(2, "@two"),
      3 -> User(3, "@three"),
      4 -> User(4, "@four")
    )

    val postDatabase: Map[PostId, Post] = Map(
      "String1" -> Post("String1", "@one"),
      "String2" -> Post("String2", "@two"),
      "String3" -> Post("String3", "@three"),
      "String4" -> Post("String4", "@four")
    )

    object Users extends Data[UserId, User] {
      def name = "Users"

      def source[F[_] : ConcurrentEffect]: DataSource[F, UserId, User] = new DataSource[F, UserId, User] {
        override def data = Users

        override def CF = ConcurrentEffect[F]

        override def fetch(id: UserId): F[Option[User]] =
          latency[F](s"One Post $id") >> CF.pure(userDatabase.get(id))

        override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
          latency[F](s"Batch Posts $ids") >> CF.pure(userDatabase.filterKeys(ids.toList.toSet))
      }
    }

    object Posts extends Data[PostId, Post] {
      def name = "Posts"

      def source[F[_] : ConcurrentEffect]: DataSource[F, PostId, Post] = new DataSource[F, PostId, Post] {
        override def data = Posts

        override def CF = ConcurrentEffect[F]

        override def fetch(id: PostId): F[Option[Post]] =
          latency[F](s"One User $id") >> CF.pure(postDatabase.get(id))

        override def batch(ids: NonEmptyList[PostId]): F[Map[PostId, Post]] =
          latency[F](s"Batch Users $ids") >> CF.pure(postDatabase.filterKeys(ids.toList.toSet))
      }
    }

    def getUser[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
      Fetch(id, Users.source)

    def getUsers[F[_] : ConcurrentEffect](ids: UserId*): Fetch[F, List[User]] =
      ids.toList.traverse(getUser[F])

    def getLotsUsers[F[_] : ConcurrentEffect]: Fetch[F, (User, User, List[User])] = for {
      user1 <- getUser(1)
      user2 <- getUser(2)
      users <- getUsers(3, 3, 4)
    } yield (user1, user2, users)

    def getPost[F[_] : ConcurrentEffect](id: PostId): Fetch[F, Post] =
      Fetch(id, Posts.source)

    def getPosts[F[_] : ConcurrentEffect](ids: PostId*): Fetch[F, List[Post]] =
      ids.toList.traverse(getPost[F])

    def getLotsPosts[F[_] : ConcurrentEffect]: Fetch[F, (Post, Post, List[Post])] = for {
      post1 <- getPost("String1")
      post2 <- getPost("String2")
      posts <- getPosts("String3", "String3", "String4")
    } yield (post1, post2, posts)

    def fetchMulti[F[_]: ConcurrentEffect] =
      (getLotsPosts, getLotsUsers).tupled

    for {
      _ <- Fetch.run[IO](fetchMulti)
    } yield ExitCode.Success
  }
}