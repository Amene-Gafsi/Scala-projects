package calculator

import scala.util.{Try, Success, Failure}

object FullEvaluator:
  /** Result of evaluation. */
  enum FullEvalResult:
    case Ok(v: Double)
    case DivByZero
    case UndefinedVar(name: String)

    def get: Double = this match
      case Ok(v)              => v
      case DivByZero          => throw new RuntimeException("division by zero")
      case UndefinedVar(name) => throw new RuntimeException(s"undefined variable: $name")

  // Define your own context here
  enum MyContext:
    case Empty
    case Cons(name: String, value: Double, tail: MyContext)

  type Ctx = MyContext

  def empty: Ctx =
    MyContext.Empty

  def cons(name: String, value: Double, tail: Ctx) =
    MyContext.Cons(name, value, tail)

  def fromList(xs: List[(String, Double)]): Ctx =
    xs match
      case Nil           => empty
      case (n, v) :: rem => cons(n, v, fromList(rem))

class FullEvaluator(ctx: FullEvaluator.Ctx) extends Evaluator[FullExpr, FullEvaluator.FullEvalResult]:
  import FullEvaluator.*
  import FullExpr.*
  import FullEvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: FullExpr): FullEvalResult =
    e match
      case Number(value) => Ok(value)
      case Add(e1, e2)   => evaluateBinaryOperation(e1, e2, (v1, v2) => v1 + v2)

      case Minus(e1, e2) => evaluateBinaryOperation(e1, e2, (v1, v2) => v1 - v2)

      case Mul(e1, e2)   => evaluateBinaryOperation(e1, e2, (v1, v2) => v1 * v2)

      case Neg(e)        => evaluateBinaryOperation(Number(0), e, (v1, v2) => -v2)

      case Div(e1, e2)  => 
        if (evaluate(e2).get != 0) then evaluateBinaryOperation(e1, e2, (v1, v2) => v1 / v2)            
        else DivByZero
      
      case Var(name) => findValueOf(name, ctx)
        

  def findValueOf(name: String, ctx: FullEvaluator.Ctx): FullEvalResult =
    ctx match {
      case MyContext.Cons(n, v, tail) => if n == name then Ok(v) else findValueOf(name, tail)
      case MyContext.Empty  => UndefinedVar(name)
    }

  private def evaluateBinaryOperation(e1: FullExpr, e2: FullExpr, op: (Double, Double) => Double): FullEvalResult = 
      (evaluate(e1), evaluate(e2)) match 
        case (Ok(v1), Ok(v2)) => Ok(op(v1, v2))
        case (UndefinedVar(name), _)  => UndefinedVar(name)
        case (_, UndefinedVar(name)) => UndefinedVar(name)
        case _ => DivByZero
      
    
