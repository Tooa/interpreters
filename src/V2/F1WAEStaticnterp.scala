package V2


/**
 * In practice dynamic scoping is considered to be bad, because it gives us
 * hard times in identifying the current identifier binding. This is due to
 * the fact, that the binding is determined at runtime.
 *
 * To overcome this issue we implement static scoping. For First-Order languages
 * this can be done by passing a fresh clean environment to the function application.
 * Note, that this will not work out for first-class languages, where functions can
 * appear in Let/Let-Bindings.
 */
object F1WAEStaticnterp extends App {

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
        // Dynamic scoping
        //val extendedEnv = env + (argName -> interp(argExpr, env, funDefs))

        //Static scoping
        val funEnv = Map(argName -> interp(argExpr, env, funDefs))
        interp(bodyExpr, funEnv, funDefs)
    }
  }

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToF1WAE(symbol: Symbol) = Id(symbol)
  implicit def intToF1WAE(n: Int) = Num(n)

  val funDefs = Map(
    'f -> FunDef('n, App('g, Add('n, 5))),
    'g -> FunDef('n, Sub('n, 1)))

  assert(interp(App('f, 5), Map(), funDefs) == 9)

  val funDefs2 = Map(
    'f -> FunDef('y, Sub('y, 1)),
    'g -> FunDef('y, Sub('y, 1)),
    'f -> FunDef('x, App('g, Add('x, 3))))

  assert(interp(App('f, 10), Map(), funDefs2) == 12)

  assert(interp(
    Let('x, 3, Add(Let('x, 4, Add('x, 3)), 'x)), Map(), Map()) == 10)

  // Due to static scoping n is not bound inside the function
  try {
    interp(Let('n, 5, App('f, 10)), Map(), Map('f -> FunDef('x, 'n)))
    assert(false)
  } catch { case e: Exception => Unit }

  try {
    interp(App('f, 4), Map(), Map('f -> FunDef('y, Add('x, 'y))))
    assert(false)
  } catch { case e: Exception => Unit }

}
