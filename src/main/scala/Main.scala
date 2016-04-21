import scalaz._
import Scalaz._
import effect._
import IO._
import Validation.FlatMap._
import scala.language.implicitConversions

object Main {

  trait Simplifiable {
    def simplify: ValidationNel[String, Double]
  }

  sealed trait Operator {
    def op: (Double, Double) => Double
  }

  case object Plus   extends Operator { def op = _ + _ }
  case object Minus  extends Operator { def op = _ - _ }
  case object Times  extends Operator { def op = _ * _ }
  case object Divide extends Operator { def op = _ / _ }
  case object Modulo extends Operator { def op = _ % _ }

  sealed trait Expr

  case class OpExpr(lhs: Expr, op: Operator, rhs: Expr) extends Expr

  case class NumExpr(i: Double) extends Expr

  case class BraceExpr(lhs: String, exp: Expr, rhs: String) extends Expr

  val RE_SPACEL = " (.*)".r
  val RE_SPACER = "(.*) ".r
  val RE_BRACE  = "(.*)\\(([^\\(\\)]*)\\)(.*)".r
  val RE_DOUBLE = "([0-9]+\\.?[0-9]*)".r
  val RE_PLUS   = "(.*)\\+(.*)".r
  val RE_MINUS  = "(.*)\\-(.*)".r
  val RE_TIMES  = "(.*)\\*(.*)".r
  val RE_DIVIDE = "(.*)\\/(.*)".r
  val RE_MODULO = "(.*)\\%(.*)".r

  def toExpr(in: String): ValidationNel[String, Expr] = in match {
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
    def simplify: ValidationNel[String, Double] = expr match {
      case NumExpr(i) => i.successNel[String]
      case OpExpr(lhs, op, rhs) =>
        try {
          (lhs.simplify |@| rhs.simplify) { (l, r) =>
            op.op(l, r)
          }
        } catch {
          case e: ArithmeticException => e.getMessage.failureNel[Double]
        }
      case BraceExpr(lhs, exp, rhs) =>
        exp.simplify flatMap { s => 
          toExpr(s"$lhs $s $rhs").flatMap(_.simplify)
        }
    }
  }

  def parseLine(in: String): IO[Unit] =
    toExpr(in) flatMap { _.simplify } match {
        case Success(result) => showSuccess(result)
        case Failure(errors) => errors foldMap showError
    }

  val showSuccess: (Double) => IO[Unit] = (r) => putStrLn(s"    ✓> ${r.show}")

  val showError: (String) => IO[Unit] = (e) => putStrLn(s"    ✗> ${e.show}")

  def loop(): IO[Unit] =
    for {
      _      <- putStr("calcƶ> ")
      input  <- readLn
      exit   =  input === "exit"
      _      <- parseLine(input).whenM(!exit)
      _      <- loop.whenM(!exit)
    } yield ()

  def main(args: Array[String]): Unit =
    loop.unsafePerformIO

}

