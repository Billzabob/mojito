package mojito.fetching

import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._
import fetch._

object fetching extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    type UserId = Int
    case class User(id: UserId, username: String)

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

    object Users extends Data[UserId, User] {
      def name = "Users"

      def source[F[_] : ConcurrentEffect]: DataSource[F, UserId, User] = new DataSource[F, UserId, User] {
        override def data = Users

        override def CF = ConcurrentEffect[F]

        override def fetch(id: UserId): F[Option[User]] =
          latency[F](s"One User $id") >> CF.pure(userDatabase.get(id))

        override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
          latency[F](s"Batch Users $ids") >> CF.pure(userDatabase.filterKeys(ids.toList.toSet))
      }
    }

    def getUser[F[_] : ConcurrentEffect](id: UserId): Fetch[F, User] =
      Fetch(id, Users.source)

    def getUsers[F[_] : ConcurrentEffect](id: UserId, id2: UserId): Fetch[F, (User, User)] =
      (getUser(id), getUser(id2)).tupled


    for {
      users <- Fetch.run[IO](getUsers(1, 2))
      _ <- IO(println(users._1))
      _ <- IO(println(users._2))
    } yield ExitCode.Success
  }
}