package mojito.free

import cats.data.Kleisli
import cats.implicits._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

object FreeExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    final case class Obj[F[_], A, B](fields: List[Field[F, A, B]])

    final case class Field[F[_], A, B](resolve: Kleisli[F, A, B])

    val foo: Kleisli[IO, Int, Long] = Kleisli((a: Int) => IO(a.toLong))
    val foo2: Kleisli[IO, Int, String] = Kleisli((a: Int) => IO(s"foo2: $a"))

    val hoo = (foo, foo2).parMapN(_.toString + _)

    println(hoo(10).unsafeRunSync())

    IO(ExitCode.Success)
  }
}
