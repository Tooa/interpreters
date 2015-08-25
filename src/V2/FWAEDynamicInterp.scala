package V2


object FWAEDynamicInterp extends App {
  sealed abstract class FWAE

  case class Num(n: Int) extends FWAE
  case class Sub(lhs: FWAE, rhs: FWAE) extends FWAE
  case class Add(lhs: FWAE, rhs: FWAE) extends FWAE
  case class Let(boundId: Symbol, namedExpr: FWAE, boundBody: FWAE) extends FWAE
  case class Id(name: Symbol) extends FWAE
  case class Fun(param: Symbol, body: FWAE) extends FWAE
  // No, external funName anymore.
  case class App(funExpr: FWAE, arg: FWAE) extends FWAE

  abstract class Value
  case class VNum(n: Int) extends Value
  case class VFun(param: Symbol, body: FWAE) extends Value

  // We use environments again, but this time it can point to VNum oder VFun
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
    case Fun(param, body) => VFun(param, body)
    case App(funExpr, argExpr) => interp(funExpr, env) match {
      case VFun(param, body) =>
        // Dynamic scoping here
        val extendedEnv = env + (param -> interp(argExpr, env))
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
      Let('f, Fun('y, Add('x, 'y)),
        Let('x, 5, App('f, 4))))) == VNum(9)) // cf. PLAI page 62

  assert(interp(
    Let('x, 3,
      Fun('y, Add('x, 'y)))) == VFun('y, Add('x, 'y))) // cf. PLAI page 62

  assert(interp(
    Let('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5)))) == VNum(11))

  assert(interp(
    Let('x, 2, App(Let('x, 5, Fun('x, Add('x, 'x))), 'x))) == VNum(4))
}
