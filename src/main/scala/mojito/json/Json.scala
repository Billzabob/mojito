package mojito.json

sealed trait Json[T] extends Product with Serializable

object Json {

  // TODO: Use matryoshka here. Should help with figuring out he other stuff.
  final case class JsonObj[T](values: Map[String, T]) extends Json[T]

  final case class JsonArray[T](values: List[T]) extends Json[T]

  final case class JsonString[T](value: String) extends Json[T]

  final case class JsonNumber[T](value: Double) extends Json[T]

  final case class JsonBoolean[T](value: Boolean) extends Json[T]

  // TODO: This seems weird, but case object can't take a type parameter.
  final case class JsonNull[T]() extends Json[T]

}

object testing extends App {
  import matryoshka._
  import matryoshka.implicits._
  import scalaz.Functor

  sealed trait Expr[T] extends Product with Serializable

  final case class Num[T](num: Int) extends Expr[T]

  final case class Mul[T](l: T, r: T) extends Expr[T]

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B) = fa match {
      case Num(num) => Num(num)
      case Mul(l, r) => Mul(f(l), f(r))
    }
  }

  val algebra: Algebra[Expr, Int] = {
    case Num(num) => num
    case Mul(l, r) => l * r
  }

  def foo[T](implicit T: Corecursive.Aux[T, Expr]): T = Mul(Num[T](2).embed, Num[T](3).embed).embed

  import matryoshka.data.Mu

  val result = foo[Mu[Expr]].cata(algebra)

  println(result)
}
