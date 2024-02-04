package calculator

object SmallStepEvaluator:
  import TinyExpr.*

  /** Evaluate the expression by one step. Return the expression as it is if it
    * has been fully evaluated.
    */
  def step(e: TinyExpr): TinyExpr = e match {
  case Number(value) => e
  case Add(e1, e2) =>
    (e1, e2) match {
      case (Number(v1), Number(v2)) => Number(v1 + v2)
      case (a, b) if isNumber(a) => Add(a, step(b))
      case (a, b) if isNumber(b) => Add(step(a), b)
      case _ => Add(step(e1), e2)
    }
  case Minus(e1, e2) =>
    (e1, e2) match {
      case (Number(v1), Number(v2)) => Number(v1 - v2)
      case (a, b) if isNumber(a) => Minus(a, step(b))
      case (a, b) if isNumber(b) => Minus(step(a), b)
      case _ => Minus(step(e1), e2)
    }
  case Mul(e1, e2) =>
    (e1, e2) match {
      case (Number(v1), Number(v2)) => Number(v1 * v2)
      case (a, b) if isNumber(a) => Mul(a, step(b))
      case (a, b) if isNumber(b) => Mul(step(a), b)
      case _ => Mul(step(e1), e2)    }
  case Neg(e) => 
    e match
      case Number(v) => Number(-v)
      case _ => Neg(step(e))

    
}

  def isNumber(e: TinyExpr): Boolean = 
    e match 
      case Number(_) => true
      case _ => false
        