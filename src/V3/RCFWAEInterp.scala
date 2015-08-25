package V3


/**
 * From Let to LetRec (Rec)
 *
 * Does recursion require a new concept? Consider the following example:
 *
 * Let('fact, Fun('n, If0('n, 1, Mult('n, App('fact, Sub('n, 1))))),
 * App('fact, 5))) == NumV(120))
 *
 * val extendedEnv = env + (boundId -> interp(namedExpr, env))
 * interp(boundBody, extendedEnv)
 *
 * Let binds the identifier 'fact only in the boundBody (-> interp(boundBody, extendedEnv)).
 * When interpreting the namedExpr (-> interp(namedExpr, env)) the passed env lacks of a
 * definition for 'fact.
 *
 * This is why we need to add a new LetRec (Rec) construct that binds the boundId (here 'fact)
 * to both, the expression and the boundBody!
 *
 * Rec('fact, Fun('n, If0('n, 1, Mult('n, App('fact, Sub('n, 1))))),
 *     App('fact, 5))) == NumV(120))
 *
 *
 * This can done by using cyclic environments:
 *
 * val recEnv = collection.mutable.Map() ++ env
 * recEnv += boundId -> interp(namedExpr, recEnv)
 *
 * The call interp(namedExpr, recEnv) will return a Closure that
 * closes over the recEnv. In addition we bind 'fact to the recEnv.
 *
 */
object RCFWAEInterp extends App {

  sealed abstract class Expr

  case class Num(n: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr
  case class With(boundId: Symbol, namedExpr: Expr, boundBody: Expr) extends Expr
  case class Id(name: Symbol) extends Expr
  case class Fun(param: Symbol, body: Expr) extends Expr
  case class If0(testExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class Rec(boundId: Symbol, namedExpr: Expr, boundBody: Expr) extends Expr
  case class App(funExpr: Expr, argExpr: Expr) extends Expr
  type Env = scala.collection.Map[Symbol, Val]

  sealed abstract class Val

  case class NumV(n: Int) extends Val

  case class Closure(param: Symbol, body: Expr, env: Env) extends Val

  def interp(expr: Expr, env: Env = Map()): Val = expr match {
    case Num(n) => NumV(n)

    case Add(lhs, rhs) => {
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => sys.error("can only add numbers, but got: " +(lhsV, rhsV))
      }
    }

    case Sub(lhs, rhs) => {
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
        case _ =>
          sys.error("can only subtract numbers, but got: " +(lhsV, rhsV))
      }
    }

    case Mult(lhs, rhs) => {
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ =>
          sys.error("can only multiply numbers, but got: " +(lhsV, rhsV))
      }
    }

    case If0(testExpr, thenExpr, elseExpr) => interp(testExpr, env) match {
      case NumV(0) => interp(thenExpr, env)
      case NumV(_) => interp(elseExpr, env)
      case _ => sys.error("Can only test numbers.")
    }
    case With(boundId, namedExpr, boundBody) =>
      val extendedEnv = env + (boundId -> interp(namedExpr, env))
      interp(boundBody, extendedEnv)
    case Id(name) => env(name)
    case Fun(param, body) => Closure(param, body, env)
    case Rec(boundId, namedExpr, boundBody) =>
      val recEnv = collection.mutable.Map() ++ env
      recEnv += (boundId -> interp(namedExpr, recEnv))
      interp(boundBody, recEnv)
    case App(funExpr, argExpr) => interp(funExpr, env) match {
      case Closure(param, body, funEnv) =>
        interp(body, funEnv + (param -> interp(argExpr, env)))
      case v => sys.error("Can only call functions, but got " + v)
    }
  }

  // Some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def idToExpr(id: Symbol) = Id(id)
  implicit def numToExpr(n: Int) = Num(n)

  assert(interp(
    Rec('fact, Fun('n, If0('n, 1, Mult('n, App('fact, Sub('n, 1))))),
      App('fact, 5))) == NumV(120))
  assert(interp(
    With('x, 3, Fun('y, Add('x, 'y)))) ==
    Closure('y, Add('x, 'y), Map('x -> NumV(3))))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5)))) == NumV(11))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)), 'inc)) == Closure('x, Add('x, 1), Map()))
  assert(interp(With('x, 3, App(Fun('y, Add('x, 'y)), 4))) == NumV(7))
}
