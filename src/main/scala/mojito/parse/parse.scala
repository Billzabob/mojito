package mojito
package parse

import atto._
import Atto._
import atto.ParseResult.Done
import cats.data.NonEmptyList
import cats.implicits._

object parse extends App {
  def token[A](p: Parser[A]): Parser[A] =
    p <~ ignoredSpace named "token"

  def bracket[A, B, C](left: Parser[B], p: => Parser[A], right: => Parser[C]): Parser[A] =
    token(left) ~> p <~ token(right)

  def braces[A](p: => Parser[A]): Parser[A] = {
    bracket(char('{'), p, char('}')) named "braces"
  }

  def parens[A](p: => Parser[A]): Parser[A] =
    bracket(char('('), p, char(')')) named "parens"

  def squareBrackets[A](p: => Parser[A]): Parser[A] = {
    bracket(char('['), p, char(']')) named "square brackets"
  }

  val tab = char('\t')

  val newline = char('\n')

  val carriageReturn = char('\r')

  val space = char(' ')

  val sourceChar = tab | newline | carriageReturn | charRange(' ' to '\uFFFF')

  val unicodeBom = char('\uFEFF')

  val whitespace = tab | space

  val lineTerminator = string("\r\n") | newline -| (_.toString)

  val commentChar = tab | charRange(' ' to '\uFFFF')

  val comment = char('#') ~ stringOf(commentChar) -| { case (a, b) => a.toString + b } named "comment"

  val comma = char(',')

  val ignored = (unicodeBom | whitespace | comma) -| (_.toString) | lineTerminator | comment named "ignored"

  val ignoredSpace = many(ignored).void named "ignored space"

  val underscore = char('_')

  val colon = token(char(':'))

  val letter = charRange('A' to 'Z') | charRange('a' to 'z')

  val digit = charRange('0' to '9')

  val nameFirst = underscore | letter

  val nameRest = nameFirst | digit

  val name = token(nameFirst ~ stringOf(nameRest) -| { case (a, b) => a.toString + b } named "name")

  val operationType = string("query") | string("mutation") | string("subscription")

  def variableDefinitions = ???

  def directives = ???

  val alias = name <~ colon named "alias"

  val nonZeroDigit = charRange('1' to '9') named "non-zero digit"

  val sig = char('-') >| -1 | ok(1) named "negative sign"

  val sigNum = (sig, nonZeroDigit, stringOf(digit)).mapN { case (a, b, c) => a * BigInt(b.toString + c) } named "signed number"

  val intPart = sig ~> char('0') >| BigInt(0) | sigNum named "int part"

  val fracPart = char('.') ~> stringOf1(digit).map(BigInt(_))

  val expIndicator = char('e') | char('E')

  val expPart = (expIndicator ~> sig, stringOf1(digit)).mapN { case (a, b) => a * BigInt(b) }

  sealed trait Value extends Product with Serializable

  final case class IntValue(value: BigInt) extends Value

  final case class FloatValue(value: BigDecimal) extends Value

  final case class Variable(name: String) extends Value

  final case class BoolValue(bool: Boolean) extends Value

  final case object NullValue extends Value

  final case class EnumValue(name: String) extends Value

  final case class ListValue(values: List[Value]) extends Value

  val variable = token(char('$')) ~> name -| Variable.apply named "variable"

  val intValue = token(intPart -| IntValue.apply) named "integer value"

  val floatPart = (intPart, fracPart).mapN { case (a, b) => BigDecimal(s"$a.$b") }

  val number = (floatPart | (intPart -| BigDecimal.apply), expPart).mapN { case (a, b) => BigDecimal(s"${a}e$b") }

  val floatValue = token((number | floatPart) -| FloatValue.apply) named "float value"

//  def stringValue = token(???)

  val keywords = "true" :: "false" :: "null" :: Nil

  val enumValue = name.filter(a => !keywords.contains(a)) -| EnumValue.apply named "enum value"

  val booleanValue = token((string("true") >| true | string("false") >| false) -| BoolValue.apply) named "boolean value"

  val nullValue = token(string("null") >| NullValue) named "null value"

  lazy val listValue = squareBrackets(many(value)) -| ListValue.apply named "list value"

  def objectValue = ???

  val value: Parser[Value] = {
    floatValue.widen[Value] |
    intValue.widen[Value] |
    variable.widen[Value] |
    enumValue.widen[Value] |
    booleanValue.widen[Value] |
    nullValue.widen[Value] |
    listValue.widen[Value]
  } named "value"

  final case class Argument(name: String, value: Value)

  val argument = (name <~ colon, value).mapN(Argument.apply) named "argument"

  val arguments = parens(many1(argument)) named "arguments"

  final case class Field(alias: Option[String], name: String, arguments: Option[NonEmptyList[Argument]], selectionSet: Option[NonEmptyList[Field]])

  val field: Parser[Field] = (
    opt(alias),
    name,
    opt(arguments),
    //        opt(directives),
    opt(selectionSet)
  ).mapN(Field.apply) named "field"

  val selection = field named "selection" //| fragmentSpread | inlineFragment

  lazy val selectionSet = braces(many1(selection)) named "selection set"

  sealed trait Definition extends Product with Serializable

  sealed trait ExecutableDefinition extends Definition

  final case class Operation(opType: String,
                             name: Option[String],
                             //                       variableDefinitions: Option[String],
                             //                       directives: Option[String],
                             selectionSet: NonEmptyList[Field]) extends ExecutableDefinition

  val operationDefinition = (
    operationType,
    opt(name),
    //    opt(variableDefinitions),
    //    opt(directives),
    selectionSet
  ).mapN(Operation.apply) | selectionSet -| (a => Operation("query", None, a)) named "operation"

  val executableDefinition: Parser[ExecutableDefinition] = operationDefinition -| (e => e: ExecutableDefinition) named "executable definition" //| fragmentDefinition

  val definition: Parser[Definition] = executableDefinition -| (d => d: Definition) named "definition" // | typeSystemDefinition | typeSystemExtension

  final case class Document(definitions: NonEmptyList[Definition])

  val document = many1(definition) -| Document.apply named "document"

  val test =
    """{  #   testing
      |  hi
      |  (
      |  test
      |  :
      |  [
      |  1
      |  1.12e53
      |  ]
      |  )
      |  {
      |  hi
      |  }
      |}""".stripMargin

  val Done(remaining, result) = document.parse(test).done

  println(s"result: $result")
  println(s"""remaining: "$remaining"""")
}
