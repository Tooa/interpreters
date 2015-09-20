// This file contains an interpreter for CFWAE/L with caching
package lazy_eval

/**
	Our current implementation will evaluate an expression multiple times,
	if we require it from the env multiple times. This implementation
	will add a cache of type value to the EClosure. This cache will contain
	the evaluated expression, once we call strict (forcing the evaluation) 
  on it.

	This does not always work. Consider a language where functions
	could have side-effects e.g getBalance(). Storing the result
	of such a expression would not be correct with the state changes.

	-> Caching implies lack of side effects

	Any language that caches computations (eager or lazy) is making 
	the assumption that an expression evaluates to the same value
	every time it evaluates.

	Why are most languages strict?
  ------------------------------
	
	Creating and maintaining expresssion closures contributes to 
	increased execution overhead (space complx, time complx e.g 
	time force closure, time create closure ...)

	But, the reason is not really efficiency.

	* In a purely functional language, one is concerned with
	results, not how they are computed.

	* Lazy evaluation is attractive, especially given that its
	termination properties are better than call-by-value.

	* But, programs in lazy languages are not islands: Programs
    must eventually interact with the world which itself has
    true side-effects!

  Conclusion:

	Any language with side-effects must have a
  transparent, easy-to-understand order of evaluation.

	Execution order under lazy evaluation is not easy to
  understand: It can be counterintuitive and does not mix
  well with exceptions, concurrency, input/output.
**/
object CFWAELazyCachingInterp extends App {
  sealed abstract class CFWAE
  case class Num(n: Int) extends CFWAE
  case class Add(lhs: CFWAE, rhs: CFWAE) extends CFWAE
  case class Sub(lhs: CFWAE, rhs: CFWAE) extends CFWAE
  case class With(name: Symbol, namedExpr: CFWAE, body: CFWAE) extends CFWAE
  case class Id(name: Symbol) extends CFWAE
  case class Fun(param: Symbol, body: CFWAE) extends CFWAE
  case class App(funExpr: CFWAE, argExpr: CFWAE) extends CFWAE
  case class If0(test: CFWAE, posBody: CFWAE, negBody: CFWAE) extends CFWAE

  type Env = Map[Symbol, Value]

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class FClosure(param: Symbol, body: CFWAE, env: Env) extends Value
  case class EClosure(expr: CFWAE, env: Env, var cache: Option[Value] = None)
    extends Value

  def interp(expr: CFWAE, env: Env = Map()): Value = expr match {
    case Num(n) => NumV(n)

    case Add(lhs, rhs) => {
      val lhsV = strict(interp(lhs, env))
      val rhsV = strict(interp(rhs, env))
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
      }
    }

    case Sub(lhs, rhs) => {
      val lhsV = strict(interp(lhs, env))
      val rhsV = strict(interp(rhs, env))
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
        case _ =>
          sys.error("can only subtract numbers, but got: " + (lhsV, rhsV))
      }
    }

    case With(boundId, namedExpr, boundBody) => {
      interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))
    }

    case Id(name) => strict(env(name))

    case Fun(arg, body) => FClosure(arg, body, env)

    case If0(testExpr, thenExpr, elseExpr) => {
      val testV = strict(interp(testExpr, env))
      testV match {
        case NumV(n) => {
          if (n == 0)
            interp(thenExpr, env)
          else
            interp(elseExpr, env)
        }
        case _ => sys.error("can only test numbers, but got: " + testV)
      }
    }

    case App(funExpr, argExpr) => {
      val funV = strict(interp(funExpr, env))
      funV match {
        case FClosure(fParam, fBody, fEnv) => {
          interp(fBody, fEnv + (fParam -> EClosure(argExpr, env)))
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }
  }

  def strict(value: Value): Value = value match {
    case e @ EClosure(expr, env, cache) => {
      if (cache.isEmpty) {
        e.cache = Some(strict(interp(expr, env)))
      }
      e.cache.get
    }
    case _ => value
  }

  // Some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def idToExpr(id: Symbol) = Id(id)
  implicit def numToExpr(n: Int) = Num(n)

  assert(interp(With('f, App('undef, 'x), 4)) == NumV(4))
  assert(interp(
    With('x, Add(4, 5),
      With('y, Add('x, 'x),
        With('z, 'y,
          With('x, 4, 'z))))) == NumV(18))
  assert(interp(With('x, 4, Fun('y, Add('x, 'y)))).isInstanceOf[FClosure])
  assert(interp(With('f, Fun('y, Add('x, 'y)), 'f)).isInstanceOf[FClosure])
}
