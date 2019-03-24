package mojito.fetching

import cats.effect._
import cats.implicits._
import fetch._

object fetching extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

    def website[F[_] : ConcurrentEffect] = Website("Title").user[F](1).flatMap(_.post[F]("1"))

    for {
      _ <- Fetch.run[IO](website)
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
