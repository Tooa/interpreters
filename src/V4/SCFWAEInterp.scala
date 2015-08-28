package V4


/**
 * Stateful interpreter with sequencing. The concept of sequencing and state requires us to introduce
 * an additional store. Consider the following example:
 *
 * case Seqn(e1, e2) => {
 *   interp(e1, env)
 *   interp(e2, env)
 * }
 *
 * With the current approach, there is no way the interpretation of the first sequent
 * can affect the interpretation of the second.
 *
 * Could we use environments that flow through interpreter calls as the medium for chaining the mutation effects?
 *
 * case Seqn(e1, e2) => {
 *  val (res, env1) = interp(e1, env)
 *  interp(e2, env1)
 * }
 *
 * Resulting Problems:
 * --------------------
 *
 * Seqn(
 *  With('x, 10, 'x),
 *  'x)
 *
 *  The new "environment-passing style" of interpretation above tells that x in the second expression in sequence
 *  will be evaluated in an environment that binds x to 10. This contradicts static scoping (!), which tells us that x
 *  in the second expression is unbound
 *
 * With('a, NewBox(1),
 *   With('f, Fun('x, Add('x, OpenBox('a))),
 *     Seqn(
 *       SetBox('a, 2),
 *       App('f, 5))))
 *
 * Well, the problem again is that our static scoping rules for function application will defeat environment-passing style.
 * If we pass the updated env to the function application, this will be ignored since f closes the body in the
 * environment at definition time (closure captures the env when interpreting the function binding).
 *
 * Solution:
 *
 * We need two repositories for information:
 *    * The environment for statically scoped bindings.
 *    * The store for tracking dynamic changes.
 * 
 * These two data structures reflect the differences between identifiers bound in the environment
 * with static scope and values that have (potentially indefinite) dynamic extent. The store is
 * a global record of changes made during execution. Closures however capture the environment at 
 * function definition, but not the store (preserving of static scoping). 
 * 
 * Static scope of variables versus dynamic extent of values.
 * 	> Environments as repositories for static scope.
 *  > Stores as repositories for dynamic extent.
 */
object SCFWAEInterp extends App {

  sealed abstract class SCFWAE
  case class Num(n: Int) extends SCFWAE
  case class Add(lhs: SCFWAE, rhs: SCFWAE) extends SCFWAE
  case class Let(boundId: Symbol, namedExpr: SCFWAE, boundBody: SCFWAE) extends SCFWAE
  case class Id(name: Symbol) extends SCFWAE
  case class If0(test: SCFWAE, thenExpr: SCFWAE, elseExpr: SCFWAE) extends SCFWAE
  case class Fun(param: Symbol, body: SCFWAE) extends SCFWAE
  case class App(funExpr: SCFWAE, argExpr: SCFWAE) extends SCFWAE
  case class Seqn(e1: SCFWAE, e2: SCFWAE) extends SCFWAE
  case class SetId(id: Symbol, valueExpr: SCFWAE) extends SCFWAE
  case class NewBox(boxExpr: SCFWAE) extends SCFWAE
  case class SetBox(boxExpr: SCFWAE, valueExpr: SCFWAE) extends SCFWAE
  case class OpenBox(boxExpr: SCFWAE) extends SCFWAE

  type Location = Int
  var _currentLocation = 0
  def nextLocation: Location = {
    _currentLocation += 1
    _currentLocation
  }

  type Env = Map[Symbol, Location]
  type Store = Map[Location, Val]

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: Symbol, body: SCFWAE, env: Env) extends Val
  case class Box(location: Location) extends Val

  def interp(
              expr: SCFWAE,
              env: Env = Map(),
              store: Store = Map()): (Val, Store) = expr match {

    case Num(n) => (NumV(n), store)
    // We use left-to-right evaluation here
    // With('b, 4,
    //  Add('b,
    //    Seqn(
    //      SetId('b, 5),
    //      'b)))
    // Left-to-right evaluation -> 9
    // Right-to-left evaluation -> 10
    case Add(lhs, rhs) =>
      val(lhsv, s1) = interp(lhs, env, store)
      lhsv match {
        case NumV(n1) =>
          val(rhsv, s2) = interp(rhs, env, s1)
          rhsv match {
            case NumV(n2) => (NumV(n1 + n2), s2)
            case _ => sys.error("Can only add numbers.")
          }
        case _ => sys.error("Can only add numbers.")
      }
    // Variables will be stored in the store. We use the env for static scoping
    case Let(boundId, namedExpr, boundBody) =>
      val(namedVal, s1) = interp(namedExpr, env, store)
      val nextLoc = nextLocation
      interp(boundBody, env + (boundId -> nextLoc), s1 + (nextLoc -> namedVal))
    case Id(name) => (store(env(name)), store)
    case If0(test: SCFWAE, thenExpr: SCFWAE, elseExpr: SCFWAE) =>
      interp(test, env, store) match {
        case (NumV(0), s1) => interp(thenExpr, env, s1)
        case (NumV(n), s1) => interp(elseExpr, env, s1)
        case (_, _) => sys.error("Can only test numbers.")
      }
    case Fun(param, body) => (Closure(param, body, env), store)
    // Function-before-argument execution order
    // We also implement call-by-value passing style
    case App(funExpr, argExpr) =>
      val(funV, funStore) = interp(funExpr, env, store)
      val(argV, argStore) = interp(argExpr, env, funStore)
      funV match {
        case Closure(fParam, fBody, fEnv) =>
          // Copy function argument on the store
          val nextLoc = nextLocation
          interp(fBody, fEnv + (fParam -> nextLoc), argStore + (nextLoc -> argV))
        case _ => sys.error("Can only call functions.")
      }

    /*
      This alternative implements call-by-reference passing style.
      Note, that the store is not updated and we require the argExpr
      to be an identifier

      With('v, 0,
        With('f, Fun('x, SetId('x, 5)),
          Seqn(
            App('f, 'v),
            'v)))

      We use different tags to distinguish between the two kind of functions
      at application time. Functions are syntactically the same as call-by-value
      functions except for the different introduced keyword.

      case ReFun(arg, body) => (RefClosure(arg, body, env), store)

      case App(funExpr, argExpr) => {
        val (funV, funStore) = interp(funExpr, env, store)
        funV match {
          // case Closure(fParam, fBody, fEnv) => ...
          case RefClosure(fParam, fBody, fEnv) => argExpr match {
            case Id(argName) => {
              interp(fBody, fEnv + (fParam -> env(argName)), funStore)
            }
            case _ => sys.error("can only call identifiers by reference")
          }
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }*/


    case Seqn(e1: SCFWAE, e2: SCFWAE) =>
      val(v1, s1) = interp(e1, env, store)
      interp(e2, env, s1)
    // Boxes are bound to identifiers with Let e.g Let('switch, NewBox(0), ...)
    case NewBox(boxExpr) =>
      val(value, s1) = interp(boxExpr, env, store)
      val nextLoc = nextLocation
      (Box(nextLoc), s1 + (nextLoc -> value))
    // SetBox('switch, 1)
    case SetBox(boxExpr, valueExpr) =>
      val(boxV, s1) = interp(boxExpr, env, store)
      val(value, s2) = interp(valueExpr, env, s1)
      boxV match {
        case Box(loc) => (value, s2 + (loc -> value))
        case _ => sys.error("Can only set boxes.")
      }
    // (NumV(1), _) = interp(With('a, NewBox(Num(1)), OpenBox('a)))
    case OpenBox(boxExpr) =>
      val(boxV, s1) = interp(boxExpr, env, store)
      boxV match {
        case Box(loc) => (s1(loc), s1)
        case _ => sys.error("Can only open boxes.")
      }
    // SetId requires the first expression to be an identifier
    // Compare to SetBox, where any expression is allowed,
    // but it has to reduce to a box-value.
    case SetId(id, valueExpr) =>
      val(value, s1) = interp(valueExpr, env, store)
      (value, s1 + (env(id) -> value))
  }

  // Some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def idToSCFWAE(id: Symbol) = Id(id)
  implicit def numToSCFWAE(n: Int) = Num(n)

  def resetLocation: Location = {
    _currentLocation = 0
    _currentLocation
  }

  val (tv1, _) = interp(Let('a, NewBox(1), OpenBox('a)))
  assert(tv1 == NumV(1))

  val (tv2, _) = interp(
    Let('a, NewBox(1),
      Let('f, Fun('x, Add('x, OpenBox('a))),
        Seqn(SetBox('a, 2), App('f, 5)))))
  assert(tv2 == NumV(7))

  val (tv3, _) = interp(
    Let('switch, NewBox(0),
      Let('toggle,
        Fun('dummy,
          If0(OpenBox('switch),
            Seqn(SetBox('switch, 1), 1),
            Seqn(SetBox('switch, 0), 0))),
        Add(App('toggle, 42), App('toggle, 42)))))
  assert(tv3 == NumV(1))

  val (tv4, _) = interp(
    Let('switch, 0,
      Let('toggle,
        Fun('dummy,
          If0('switch,
            Seqn(SetId('switch, 1), 1),
            Seqn(SetId('switch, 0), 0))),
        Add(App('toggle, 42), App('toggle, 42)))))
  assert(tv4 == NumV(1))

  resetLocation
  val (tv5, ts5) = interp(
    App(Fun('b1, App(Fun('b2, Seqn(SetBox('b1, 6), OpenBox('b2))), NewBox(7))),
      NewBox(5)))
  assert(tv5 == NumV(7))
  assert(ts5(1) == NumV(6))

  val (tv6, _) = interp(
    Let('b, 0,
      If0(Seqn(SetId('b, 5), 'b),
        1,
        'b)))
  assert(tv6 == NumV(5))

  val (tv7, _) = interp(Let('b, 4, Add('b, Seqn(SetId('b, 5), 'b))))
  assert(tv7 == NumV(9))
}
