package calculator

object Simplifier:
  import FullExpr.*

  /** Fold constant sub-expressions in values. */
  def constfold(e: FullExpr): FullExpr = 
    e match
      case Number(_) => e 
      case Var(_)    => e 
    
      case Add(e1, e2) =>
        (constfold(e1), constfold(e2)) match 
          case (Number(v1), Number(v2)) => Number(v1 + v2)
          case _                        => Add(constfold(e1), constfold(e2)) 
        
      case Minus(e1, e2) =>
        (constfold(e1), constfold(e2)) match 
          case (Number(v1), Number(v2)) => Number(v1 - v2)
          case _                        => Minus(constfold(e1), constfold(e2)) 

      case Mul(e1, e2) =>
        (constfold(e1), constfold(e2)) match 
          case (Number(v1), Number(v2)) => Number(v1 * v2)
          case _                        => Mul(constfold(e1), constfold(e2)) 

      case Neg(e) =>
        constfold(e) match 
          case Number(v) => Number(-v) 
          case _         => Neg(constfold(e))
        
      case Div(e1, e2) =>
        (constfold(e1), constfold(e2)) match 
          case (Number(v1), Number(v2)) => Number(v1 / v2)
          case _                        => Div(constfold(e1), constfold(e2))
        


  // simplification rules
  // 1. 0 + e = e + 0 = e 
  // 2. 0 - e = -e 
  // 3. e - 0 = e 
  // 4. 0 * e = e * 0 = 0
  // 5. 1 * e = e * 1 = e
  // 6. e / 1 = e
  // 7. e - e = 0
  /** Simplifiy expressions based on the listed algebraic rules. */
  def algebraic(e: FullExpr): FullExpr =
    e match
      case Number(_) => e

      case Var(_)    => e

      case Add(e1, e2) => 
        (algebraic(e1), algebraic(e2)) match
          case (Number(0), v) => v
          case (v, Number(0)) => v
          case _ => Add(algebraic(e1), algebraic(e2)) 

      case Minus(e1, e2) =>
        (algebraic(e1), algebraic(e2)) match
          case (v1, v2) if v1 == v2 => Number(0)
          case (Number(0), v) => Neg(v)
          case (v, Number(0)) => v
          case _ => Minus(algebraic(e1), algebraic(e2))                 

      case Mul(e1, e2) => 
        (algebraic(e1), algebraic(e2)) match
          case (Number(0), _) => Number(0)
          case (_, Number(0)) => Number(0)
          case (Number(1), v) => v
          case (v , Number(1)) => v
          case _ => Mul(algebraic(e1), algebraic(e2))  
    

      case Neg(e) => Neg(algebraic(e))

      case Div(e1, e2) => 
        (algebraic(e1), algebraic(e2)) match
          case (v, Number(1)) => v
          case _ => Div(algebraic(e1), algebraic(e2))

        

  def simplify(e: FullExpr): FullExpr =
    algebraic(constfold(algebraic(e)))
