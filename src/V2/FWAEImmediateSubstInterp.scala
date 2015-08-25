package V2


/**
 * First-Class function interpreter using lazy substitution. Functions are real values,
 * can be passed around, stored in data structures or returned (See VFun).
 */
object FWAEImmediateSubstInterp extends App {

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

  def subst(expr: FWAE, substId: Symbol, value: FWAE): FWAE = expr match {
    case Num(n) => expr
    case Sub(lhs, rhs) => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Add(lhs, rhs) => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Let(boundId, namedExpr, boundBody) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if(boundId == substId) {
        Let(boundId, substNamedExpr, boundBody)
      } else {
        Let(boundId, substNamedExpr, subst(boundBody, substId, value))
      }
    case Id(name) => if(name == substId) value else expr
    // Fun('x, Fun('x, x + 1)) 1 2
    // -> Fun('x, x + 1) 2
    // -> 3
    case Fun(param, body) =>
      // We could also have shadowing behaviour with functions now
      if(param == substId) Fun(param, body) else Fun(param, subst(body, substId, value))
    case App(funExpr, argExpr) => App(subst(funExpr, substId, value), subst(argExpr, substId, value))
  }

  def interp(expr: FWAE): Value = expr match {
    case Num(n) => VNum(n)
    case Sub(lhs, rhs) => interp(lhs) match {
      case VNum(n1) => interp(rhs) match {
        case VNum(n2) => VNum(n1 - n2)
        case _ => sys.error("Could only sub numbers for rhs.")
      }
      case _ => sys.error("Could only sub numbers for lhs.")
    }
    case Add(lhs, rhs) => interp(lhs) match {
      case VNum(n1) => interp(rhs) match {
        case VNum(n2) => VNum(n1 + n2)
        case _ => sys.error("Could only add numbers for rhs.")
      }
      case _ => sys.error("Could only add numbers for lhs.")
    }
    case Let(boundId, namedExpr, boundBody) =>
      // We apply lazy substitution here. This means namedExpr will be evaluated once it is needed
      interp(subst(boundBody, boundId, namedExpr))
    case Id(name) => sys.error("Found unbound identifier " + name)
    case Fun(param, body) =>
      // Functions are values and could be returned
      VFun(param, body)
    case App(funExpr, argExpr) => interp(funExpr) match {
      case VFun(param, body) => interp(subst(body, param, argExpr))
      case e => sys.error("Can only call functions but got" + e)
    }
  }

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToFWAE(symbol: Symbol) = Id(symbol)
  implicit def intToFWAE(n: Int) = Num(n)

  assert(interp(
    Let('x, 3, Fun('y, Add('x, 'y)))) ==
    VFun('y, Add(Num(3), Id('y))))
  assert(interp(
    Let('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5)))) == VNum(11))
  assert(interp(App(Let('x, 3, Fun('y, Add('x, 'y))), Add(4, 5))) == VNum(12))
}
