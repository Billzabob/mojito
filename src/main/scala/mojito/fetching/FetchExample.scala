package mojito.fetching

import cats.effect._
import cats.implicits._

object FetchExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

//    def usersForWebsite[F[_] : ConcurrentEffect](website: Website): Fetch[F, List[User]] =
//      website.allUsers[F]
//
//    def postsForUser[F[_] : ConcurrentEffect](user: User): Fetch[F, List[Post]] =
//      user.allPosts[F]

    for {
      _ <- IO(println("Starting"))
//      _ <- Fetch.run[IO](postsForUsers)
      _ <- IO(println("Done"))
    } yield ExitCode.Success
  }
}

trait FakeLatency {
  def latency[F[_] : ConcurrentEffect](msg: String): F[Unit] = for {
    _ <- Sync[F].delay(println(s"--> [${Thread.currentThread.getId}] $msg"))
    _ <- Sync[F].delay(Thread.sleep(100))
    _ <- Sync[F].delay(println(s"<-- [${Thread.currentThread.getId}] $msg"))
  } yield ()
}
