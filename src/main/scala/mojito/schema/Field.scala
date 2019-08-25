package mojito.schema

import cats.Applicative
import cats.data.{Kleisli, ValidatedNec}
import cats.effect.IO
import cats.implicits._
import mojito.json.Json
import mojito.json.Json._
// import cats.temp.par._ // TODO: Make requests in parallel

final case class Entry[F[_], A](name: String)(resolver: A => F[Json]) {
  val namedResolver: (String, Kleisli[F, A, Json]) = (name, Kleisli(resolver))
}

final case class GqlObj[F[_], A](name: String, fields: List[Entry[F, A]]) extends {

  // TODO: Probably more efficient to turn the list into a map sooner rather than later
  def resolve(request: List[String])(implicit F: Applicative[F]): ValidatedNec[String, Kleisli[F, A, Json]] = {
    val validatedEntries = request.traverse(field => fields.find(_.name === field).toValid(s"No field $field exists in object $name").toValidatedNec)
    validatedEntries.map(_.map(_.namedResolver).traverse(_.sequence).map(a => JsonObj(a.toMap)))
  }
}

object Test extends App {

  final case class Data(entry1: String, entry2: Double)

  private val schema = GqlObj[IO, Data](
    name = "Test",
    fields = List(
      Entry("entry1") { data =>
        IO(JsonString(data.entry1)).widen[Json]
      },
      Entry("entry2") { data =>
        IO(JsonNumber(data.entry2)).widen[Json]
      }
    )
  )

  private val result = schema.resolve(List("entry1", "entry2")).traverse(_.run(Data("Value1", 1.2)))

  println(result.unsafeRunSync())
}