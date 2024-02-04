package calculator

class BasicEvaluator extends Evaluator[BasicExpr, BasicEvaluator.EvalResult]:
  import BasicExpr.*
  import BasicEvaluator.*
  import EvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: BasicExpr): EvalResult =    
    e match {
      case Number(value) => EvalResult.Ok(value)
      case Add(e1, e2)   =>
        (evaluate(e1), evaluate(e2)) match {
          case (EvalResult.Ok(v1), EvalResult.Ok(v2)) => EvalResult.Ok(v1 + v2)
          case _ => EvalResult.DivByZero
        }
      case Minus(e1, e2) =>
        (evaluate(e1), evaluate(e2)) match {
          case (EvalResult.Ok(v1), EvalResult.Ok(v2)) => EvalResult.Ok(v1 - v2)
          case _ => EvalResult.DivByZero
        }
      case Mul(e1, e2)   =>
        (evaluate(e1), evaluate(e2)) match {
          case (EvalResult.Ok(v1), EvalResult.Ok(v2)) => EvalResult.Ok(v1 * v2)
          case _ => EvalResult.DivByZero
        }
      case Neg(e)        =>
        evaluate(e) match {
          case EvalResult.Ok(v) => EvalResult.Ok(-v)
          case _ => EvalResult.DivByZero
        }
      case Div(e1, e2)   =>
        (evaluate(e1), evaluate(e2)) match {
          case (EvalResult.Ok(v1), EvalResult.Ok(v2)) =>
            if (v2 != 0)
              EvalResult.Ok(v1 / v2)
            else
              EvalResult.DivByZero
          case _ => EvalResult.DivByZero
        }
    }

object BasicEvaluator:
  enum EvalResult:
    case Ok(v: Double)
    case DivByZero

    def get: Double = this match
      case Ok(v)     => v
      case DivByZero => throw new RuntimeException(s"division by zero")
