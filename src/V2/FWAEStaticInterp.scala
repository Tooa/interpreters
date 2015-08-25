package V2

/**
 * We can't apply the same trick (fresh env) we did for the F1WAE before, to enforce static scoping.
 * This is because in first-class languages, functions can appear e.g within a Let construct. Consider
 * the following example:
 *
 *    With('x, 3, App(Fun('y, Add('x, 'y)), 4))
 *
 * Using a fresh environment, would result in an unbound identifier x. To overcome this issue, we
 * introduce the concept of closures. Once a function occurs, we will create a closure wrapping the
 * function and its argument, as well as the current environment.
 */
object FWAEStaticInterp extends App {

  sealed abstract class FWAE

  case class Num(n: Int) extends FWAE
  case class Sub(lhs: FWAE, rhs: FWAE) extends FWAE
  case class Add(lhs: FWAE, rhs: FWAE) extends FWAE
  case class Let(boundId: Symbol, namedExpr: FWAE, boundBody: FWAE) extends FWAE
  case class Id(name: Symbol) extends FWAE
  case class Fun(param: Symbol, body: FWAE) extends FWAE
  case class App(funExpr: FWAE, arg: FWAE) extends FWAE

  abstract class Value
  case class VNum(n: Int) extends Value
  case class VClosure(param: Symbol, body: FWAE, env: Env) extends Value

  type Env = Map[Symbol, Value]

  def interp(expr: FWAE, env: Map[Symbol, Value] = Map()): Value = expr match {
    case Num(n) => VNum(n)
    case Sub(lhs, rhs) => interp(lhs, env) match {
      case VNum(n1) => interp(rhs, env) match {
        case VNum(n2) => VNum(n1 - n2)
        case _ => sys.error("Could only sub numbers for rhs.")
      }
      case _ => sys.error("Could only sub numbers for lhs.")
    }
    case Add(lhs, rhs) => interp(lhs, env) match {
      case VNum(n1) => interp(rhs, env) match {
        case VNum(n2) => VNum(n1 + n2)
        case _ => sys.error("Could only add numbers for rhs.")
      }
      case _ => sys.error("Could only add numbers for lhs.")
    }
    case Let(boundId, namedExpr, boundBody) =>
      // We extend the env with the new identifier binding
      val extendedEnv = env + (boundId -> interp(namedExpr, env))
      interp(boundBody, extendedEnv)
    case Id(name) => env(name)
    case Fun(param, body) => VClosure(param, body, env)
    case App(funExpr, argExpr) => interp(funExpr, env) match {
      case VClosure(param, body, funEnv) =>
        // Static scoping here, because we use the env captured when seeing the function.
        // But, we still need to bind the function parameter.
        val extendedEnv = funEnv + (param -> interp(argExpr,env))
        interp(body, extendedEnv)
      case e => sys.error("Can only call functions but got" + e)
    }
  }

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToFWAE(symbol: Symbol) = Id(symbol)
  implicit def intToFWAE(n: Int) = Num(n)

  assert(interp(
    Let('x, 3,
      Fun('y, Add('x, 'y)))) ==
    VClosure('y, Add('x, 'y), Map('x -> VNum(3))))

  assert(interp(
    Let('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5)))) == VNum(11))

  assert(interp(
    Let('inc, Fun('x, Add('x, 1)), 'inc)) == VClosure('x, Add('x, 1), Map()))

  assert(interp(Let('x, 3, App(Fun('y, Add('x, 'y)), 4))) == VNum(7))
  assert(interp(
    Let('x, 2, App(Let('x, 5, Fun('x, Add('x, 'x))), 'x))) == VNum(4))
}
