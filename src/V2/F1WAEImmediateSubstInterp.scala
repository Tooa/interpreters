package V2


/**
 * First-Order function interpreter using substitution. In First-Order languages,
 * functions are no values and can not be returned and passed around. So we introduce
 * them as separate concept.
 */
object F1WAEImmediateSubstInterp extends App {

  sealed abstract class F1WAE

  case class Num(n: Int) extends F1WAE
  case class Sub(lhs: F1WAE, rhs: F1WAE) extends F1WAE
  case class Add(lhs: F1WAE, rhs: F1WAE) extends F1WAE
  case class Let(boundId: Symbol, namedExpr: F1WAE, boundBody: F1WAE) extends F1WAE
  case class Id(name: Symbol) extends F1WAE
  case class App(funName: Symbol, arg: F1WAE) extends F1WAE

  // Note, this is not extending F1WAE, because we implement first-order
  case class FunDef(argName: Symbol, body: F1WAE)
  type FunDefs = Map[Symbol, FunDef]


  // Substitute all free instances of substId in expr with value
  def subst(expr: F1WAE, substId: Symbol, value: F1WAE): F1WAE = expr match {
    case Num(n) => expr
    case Sub(lhs, rhs) => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Add(lhs, rhs) => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Let(boundId, namedExpr, boundBody) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      // Shadowing behaviour. See package V1 subst method for more information
      if(boundId == substId) {
        Let(boundId, substNamedExpr, boundBody)
      } else {
        Let(boundId, substNamedExpr, subst(boundBody, substId, value))
      }
    case Id(name) =>
      // If identifier matches substId replace it, otherwise just return the original identifier
      if(name == substId) value else expr
    case App(funName, argExpr) => App(funName, subst(argExpr, substId, value))
  }

  def interp(expr: F1WAE, funDefs: Map[Symbol, FunDef]): Int = expr match {
    case Num(n) => n
    case Sub(lhs, rhs) => interp(lhs, funDefs) - interp(rhs, funDefs)
    case Add(lhs, rhs) => interp(lhs, funDefs) + interp(rhs, funDefs)
    case Let(boundId, namedExpr, boundBody) =>
      // We apply eager subst here. This means try to evaluate first and then subst
      interp(subst(boundBody, boundId, Num(interp(namedExpr, funDefs))), funDefs)
    case Id(name) => sys.error("Found unbound Identifier " + name)
    case App(funName, argExpr) => funDefs(funName) match {
      case FunDef(argName, bodyExpr) =>
        // Don't miss to interp the argExpr!
        interp(subst(bodyExpr, argName, Num(interp(argExpr, funDefs))), funDefs)
    }
  }

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToF1WAE(symbol: Symbol) = Id(symbol)
  implicit def intToF1WAE(n: Int) = Num(n)

  val funDefs = Map(
    'f -> FunDef('x, Sub('x, 2))
  )

  assert(interp(App('f, 10), funDefs) == 8)
  assert(interp(Let('x, 3, Add(Let('x, 4, Add('x, 3)), 'x)), Map()) == 10)

  // This doesn't work because substitution is not applied to the function definition.
  // See subst case App
  try {
    interp(Let('n, 5, App('f, 10)), Map('f -> FunDef('x, 'n)))
    assert(false)
  } catch { case e: Exception => Unit }
}
