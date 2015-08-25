package V2

/**
 * A major difficulty of substitution is the number of times we traverse
 * the source program. Substitution traverses everything - e.g unvisited
 * branches of conditionals. We come over this problem by introducing the
 * environment concept that enables fast identifier lookups. Note, we still
 * have only integers as values (First-Order language).
 *
 * In addition we will use dynamic scoping, where the scope binding for
 * an identifier is determined the by execution context at runtime. So
 * a construct like the following will be valid:
 *
 *    Let(n, 5, f(10)) where f(x) = { n }
 *
 * We will implement dynamic scoping by extending current environment for every function
 * application Let the function argument bindings.
 */
object F1WAEDynamicInterp extends App {

  sealed abstract class F1WAE

  case class Num(n: Int) extends F1WAE
  case class Sub(lhs: F1WAE, rhs: F1WAE) extends F1WAE
  case class Add(lhs: F1WAE, rhs: F1WAE) extends F1WAE
  case class Let(boundId: Symbol, namedExpr: F1WAE, boundBody: F1WAE) extends F1WAE
  case class Id(name: Symbol) extends F1WAE
  case class App(funName: Symbol, arg: F1WAE) extends F1WAE

  case class FunDef(argName: Symbol, body: F1WAE)
  type FunDefs = Map[Symbol, FunDef]
  // Our first-order language just has Int as values
  type Env = Map[Symbol, Int]

  def interp(expr: F1WAE, env: Env, funDefs: Map[Symbol, FunDef]): Int = expr match {
    case Num(n) => n
    case Sub(lhs, rhs) => interp(lhs, env, funDefs) - interp(rhs, env, funDefs)
    case Add(lhs, rhs) => interp(lhs, env, funDefs) + interp(rhs, env, funDefs)
    case Let(boundId, namedExpr, boundBody) =>
      val extendedEnv = env + (boundId -> interp(namedExpr, env, funDefs))
      interp(boundBody, extendedEnv, funDefs)
    case Id(name) =>
      // This will cause an exception if a unbound identifier (identifier not in the env) occurs
      env(name)
    case App(funName, argExpr) => funDefs(funName) match {
      case FunDef(argName, bodyExpr) =>
        val extendedEnv = env + (argName -> interp(argExpr, env, funDefs))
        interp(bodyExpr, extendedEnv, funDefs)
    }
  }

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToFWAE(symbol: Symbol) = Id(symbol)
  implicit def intToFWAE(n: Int) = Num(n)

  val funDefs = Map(
    'f -> FunDef('n, App('g, Add('n, 5))),
    'g -> FunDef('n, Sub('n, 1)))

  assert(interp(App('f, 5), Map(), funDefs) == 9)

  val funDefs2 = Map(
    'f -> FunDef('y, Sub('y, 1)),
    'g -> FunDef('y, Sub('y, 1)),
    'f -> FunDef('x, App('g, Add('x, 3))))

  assert(interp(App('f, 10), Map(), funDefs2) == 12)

  // Due to dynamic scoping the interpreter will "walk" up the "symbol-table"
  // and retrieve the binding for n
  assert(interp(Let('n, 5, App('f, 10)),
    Map(),
    Map('f -> FunDef('x, 'n))) == 5)
}
