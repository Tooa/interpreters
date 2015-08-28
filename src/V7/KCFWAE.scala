

/**
 *  First-class continuations interpreter
 *
 *
 *  Any program can be automatically transformed to a CPS
 *  version that only contains tail calls.
 *  Tail calls and TCO enable the use of functions and recursion
 *  without stack-space or run-time penalty.
 *  But, programs that only contain TCs may have a more
 *  complex structure than equivalent programs without TCs.
 *
 *	The initial value k is theoretically "the rest of Scala runtime".
 *	However, in practice we use the identity function, or a function
 *	that prints the received value and terminates.
 */
object KCFWAEInterp extends App {

  sealed abstract class KCFWAE
  case class Num(n: Int) extends KCFWAE
  case class Add(lhs: KCFWAE, rhs: KCFWAE) extends KCFWAE
  case class Sub(lhs: KCFWAE, rhs: KCFWAE) extends KCFWAE
  case class Mult(lhs: KCFWAE, rhs: KCFWAE) extends KCFWAE
  case class With(name: Symbol, namedE: KCFWAE, body: KCFWAE) extends KCFWAE
  case class Id(name: Symbol) extends KCFWAE
  case class Fun(param: Symbol, body: KCFWAE) extends KCFWAE
  case class App(funExpr: KCFWAE, arg: KCFWAE) extends KCFWAE
  case class If0(testE: KCFWAE, thenE: KCFWAE, elseE: KCFWAE) extends KCFWAE
  case class BindCC(name: Symbol, body: KCFWAE) extends KCFWAE

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class Closure(param: Symbol, body: KCFWAE, env: Env) extends Value

  type Env = Map[Symbol, Value]

  // We return the type of the continuation. This is the reason we use
  // the identity function, because the continuation is always the
  // rest of the computation, so it will contain the result of type A
  def interp[A](expr: KCFWAE, env: Env, k: (Value => A)): A = {
    /**
     * Abstract over left associative number operations
     */
    def lAssocNumOp[A](verb: String,
      op: (Int, Int) => Int,
      lhs: KCFWAE,
      rhs: KCFWAE) = {
      interp(lhs, env, (lv) => {
        interp(rhs, env, (rv) => (lv, rv) match {
		  // No tail calls before CPS trans. since they augment the original continuation
          case (NumV(n1), NumV(n2)) => k(NumV(op(n1, n2)))
          case _ => {
            sys.error("Can only %s numbers, but got %s and %s".
              format(verb, lv, rv))
          }
        })
      })
    }

    /**
     * Continuations are first class values. They are defined inside the
     * interp function to allow type erasure over A. We model continuation
     * support with scala functions.
     */
    case class Continuation(c: (Value => A)) extends Value

    expr match {
      case Num(n) => k(NumV(n))
      case Add(lhs, rhs) => lAssocNumOp("add", { _ + _ }, lhs, rhs)
      case Sub(lhs, rhs) => lAssocNumOp("subtract", { _ - _ }, lhs, rhs)
      case Mult(lhs, rhs) => lAssocNumOp("multiply", { _ * _ }, lhs, rhs)

      case With(boundId, namedExpr, body) => {
        interp(namedExpr, env, (nv) => {
          interp(body, env + (boundId -> nv), k)
        })
      }

      case Id(name) => k(env(name))

      case App(funExpr, argExpr) => {
        interp(funExpr, env, (fv) => {
          interp(argExpr, env, (argV) => fv match {
            case Closure(param, body, funEnv) => {
              interp(body, funEnv + (param -> argV), k)
            }
            // The whole point about continuations is to ignore
            // the current continuation and use the stored receiver instead.
            case Continuation(c) => c(argV)
            case _ => {
              sys.error("Can only apply closures or continuations, but got %s".
                format(fv))
            }
          })
        })
      }

      case Fun(arg, body) => k(Closure(arg, body, env))

      case If0(c, t, e) => {
        interp(c, env, (cv) => cv match {
          // Represent tail(-recursive) calls before CPS transformation
          // If a function invokes another function with the same
          // continuation that it received, it doesn't push anything to
          // the stack. This means it was actually a tail call before CPS.
          case NumV(0) => interp(t, env, k)
          case _ => interp(e, env, k)
        })
      }

      case BindCC(name, body) =>
		    // BindCC('k, App('k, 3))
        interp(body, env + (name -> Continuation(k)), k)
    }
  }

  /**
   * Little helper for easier evaluation
   */
  def eval(e: KCFWAE): Value = interp(e, Map.empty, identity)

  // some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def symbolToKCFWAE(symbol: Symbol) = Id(symbol)
  implicit def intToKCFWAE(n: Int) = Num(n)

  assert(eval(
    With('x, 3,
      Fun('y, Add('x, 'y)))) ==
    Closure('y, Add('x, 'y), Map('x -> NumV(3))))

  assert(eval(
    With('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5)))) == NumV(11))

  assert(eval(
    With('inc, Fun('x, Add('x, 1)), 'inc)) == Closure('x, Add('x, 1), Map()))

  assert(eval(With('x, 3, App(Fun('y, Add('x, 'y)), 4))) == NumV(7))

  assert(eval(
    With('x, 2, App(With('x, 5, Fun('x, Add('x, 'x))), 'x))) == NumV(4))

  // Rest of the computation is Num(3)
  assert(eval(BindCC('k, 3)) == NumV(3))

  assert(eval(BindCC('k, App('k, 3))) == NumV(3))

  // BindCC binds the current continuation k (which is the identity here)
  // to k. When App('k, 3) is executed, we call the stored continuation
  // with the result of interp(Num(3)) which is NumV(3). k(NumV(3)) is the
  // identity and will return NumV(3).
  assert(eval(BindCC('k, Add(1, App('k, 3)))) == NumV(3))


  // There is no App so the bound continuation will never be used
  // BindCC('k, 3) will simply return NumV(3)
  assert(eval(Add(1, BindCC('k, 3))) == NumV(4))

  // When we call BindCC the current continuation already contains
  // The Add case, this means we call k(NumV(3)) which will return
  // Add(NumV(3), NumV(1)) -> NumV(4)
  assert(eval(Add(1, BindCC('k, App('k, 3)))) == NumV(4))

  assert(eval(App(BindCC('k, App('k, Fun('_, 3))), 1840)) == NumV(3))

  assert(eval(BindCC('k, App('k, App('k, App('k, 3))))) == NumV(3))

  assert(eval(App(App(BindCC('k, 'k), Fun('x, 'x)), 3)) == NumV(3))
}
