// This file contains an interpreter for CFWAE/L with deferred substitution and
// static scoping. The interpreter is strict at function application,
// arithmetic ops and conditionals; only identifier lookup is non-strict.
package lazy_eval


/**
	Consider the following example:

	{let {x {+ 4 5}}
		{let {y {+ x x}}
			{let {z y}
				{let {x 4} z}}}}

	Eager evaluation (evaluate as soon a expression is bound to a variable) would return 18.
	Lazy evaluation however, returns 8 because the last occurrcence of x will override the first
	occurrence. This is somehow dynamic scoping. To preserve static scoping, we introduce EClosures
	that capture the env at execution time. 
	
	At some points we need to be strict and execute the EClosure to a value. For example
	we cant add to EClosures. We could also just apply the strict to the id case, but then
	we are not able to implement infinite data structures anymore. We are then overly strict
	for every value and evaluation. 

**/

object CFWAEAlmostStrictInterp extends App {
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
  case class EClosure(expr: CFWAE, env: Env) extends Value

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
        case _ => sys.error("can only subtract numbers, but got: " + (lhsV, rhsV))
      }
    }

    case With(boundId, namedExpr, boundBody) => {
      interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))
    }

    case Id(name) => env(name)

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
    case EClosure(expr, env) => strict(interp(expr, env))
    case _ => value
  }

  // Some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def idToExpr(id: Symbol) = Id(id)
  implicit def numToExpr(n: Int) = Num(n)

  assert(
    interp(With('x, 3, Fun('y, Add('x, 'y))))
      ==
      FClosure('y, Add('x, 'y), Map('x -> EClosure(3, Map()))))
  assert(interp(With('x, 10, Add('x, 'x))) == NumV(20))
  assert(
    interp(With('inc, Fun('x, Add('x, 1)), Add(App('inc, 4), App('inc, 5))))
      ==
      NumV(11))
  assert(interp(With('x, 3, App(Fun('y, Add('x, 'y)), 4))) == NumV(7))
  assert(interp(With('f, App('undef, 'x), 4)) == NumV(4))
  assert(interp(App(Fun('x, 3), 'y)) == NumV(3))
  assert(interp(If0(Sub(4, 4), App(Fun('x, 3), 'y), 8)) == NumV(3))
  assert(interp(
    With('x, Add(4, 5),
      With('y, Add('x, 'x),
        With('z, 'y,
          With('x, 4, 'z))))).isInstanceOf[EClosure])
}
