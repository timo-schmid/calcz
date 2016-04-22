import scalaz._
import Scalaz._
import effect._
import IO._
import Validation.FlatMap._
import scala.language.implicitConversions

object Main {

  sealed trait Expr
  case class OpExpr(lhs: Expr, op: Operator, rhs: Expr) extends Expr
  case class NumExpr(i: Double) extends Expr
  case class BraceExpr(lhs: String, exp: Expr, rhs: String) extends Expr

  trait Simplifiable { def simplify: ValidationNel[String, Double] }

  type Operator   = (Double, Double) => Double
  type Result     = ValidationNel[String, Double]
  type Expression = ValidationNel[String, Expr]

  val Plus   : Operator = _ + _
  val Minus  : Operator = _ - _
  val Times  : Operator = _ * _
  val Divide : Operator = _ / _
  val Modulo : Operator = _ % _
  
  val RE_SPACEL = " (.*)".r
  val RE_SPACER = "(.*) ".r
  val RE_BRACE  = "(.*)\\(([^\\(\\)]*)\\)(.*)".r
  val RE_DOUBLE = "([0-9]+\\.?[0-9]*)".r
  val RE_PLUS   = "(.*)\\+(.*)".r
  val RE_MINUS  = "(.*)\\-(.*)".r
  val RE_TIMES  = "(.*)\\*(.*)".r
  val RE_DIVIDE = "(.*)\\/(.*)".r
  val RE_MODULO = "(.*)\\%(.*)".r

  def toExpr(input: String): Expression = input match {
    case RE_SPACEL(sub)        => toExpr(sub)
    case RE_SPACER(sub)        => toExpr(sub)
    case RE_BRACE(l, m, r)     => toExpr(m) map { x => BraceExpr(l, x, r) }
    case RE_PLUS(lsub, rsub)   => (toExpr(lsub) |@| toExpr(rsub)) { (l, r) => OpExpr(l, Plus,   r) }
    case RE_MINUS(lsub, rsub)  => (toExpr(lsub) |@| toExpr(rsub)) { (l, r) => OpExpr(l, Minus,  r) }
    case RE_TIMES(lsub, rsub)  => (toExpr(lsub) |@| toExpr(rsub)) { (l, r) => OpExpr(l, Times,  r) }
    case RE_DIVIDE(lsub, rsub) => (toExpr(lsub) |@| toExpr(rsub)) { (l, r) => OpExpr(l, Divide, r) }
    case RE_MODULO(lsub, rsub) => (toExpr(lsub) |@| toExpr(rsub)) { (l, r) => OpExpr(l, Modulo, r) }
    case RE_DOUBLE(sub)        => NumExpr(sub.toDouble).successNel[String]
    case notAnExpr: String     => s"Not an expression: '$notAnExpr'".failureNel
  }

  implicit def toSimplifiable(expr: Expr): Simplifiable = new Simplifiable {
    def simplify: Result = expr match {
      case NumExpr(i) => i.successNel[String]
      case OpExpr(lhs, op, rhs) => (lhs.simplify |@| rhs.simplify) { (l, r) => op(l, r) }
      case BraceExpr(lhs, exp, rhs) =>
        for {
          s           <- exp.simplify
          surrounding <- toExpr(s"$lhs $s $rhs")
          simplified  <- surrounding.simplify
        } yield simplified
    }
  }

  implicit val showResult: Show[Result] = Show.shows[Result] {
    _ match {
      case Success(result) => s"    ✓> ${result.show}"
      case Failure(errors) => errors.map { e => s"    ✗> ${e.show}" }.toList.mkString("\n")
    }
  }

  def simplifyExpression(expr: Expression): Result = expr.flatMap { _.simplify }

  def loop: IO[Unit] =
    for {
      _      <- putStr("calcƶ> ")
      input  <- readLn
      done   =  input === "exit"
      expr   =  toExpr(input)            // How can i NOT do this calculation if exit was passed?
      result =  simplifyExpression(expr) // ... and that one too
      _      <- putStrLn(result.shows).whenM(!done)
      _      <- loop.whenM(!done)
    } yield ()

  def main(args: Array[String]): Unit =
    loop.unsafePerformIO

}

