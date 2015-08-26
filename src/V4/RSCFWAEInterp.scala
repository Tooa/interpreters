package V4

/**
 * This implements recursion for stateful languages.
 */
object RSCFWAEInterp extends App {

  sealed abstract class SCFWAE

  case class Num(n: Int) extends SCFWAE
  case class Add(lhs: SCFWAE, rhs: SCFWAE) extends SCFWAE
  case class Sub(lhs: SCFWAE, rhs: SCFWAE) extends SCFWAE
  case class Mult(lhs: SCFWAE, rhs: SCFWAE) extends SCFWAE
  case class Let(name: Symbol, namedExpr: SCFWAE, body: SCFWAE) extends SCFWAE
  case class LetRec(name: Symbol, namedExpr: SCFWAE, body: SCFWAE) extends SCFWAE
  case class Id(name: Symbol) extends SCFWAE
  case class If0(test: SCFWAE, posBody: SCFWAE, negBody: SCFWAE) extends SCFWAE
  case class Fun(param: Symbol, body: SCFWAE) extends SCFWAE
  case class App(funExpr: SCFWAE, argExpr: SCFWAE) extends SCFWAE
  case class Seqn(e1: SCFWAE, e2: SCFWAE) extends SCFWAE
  case class NewBox(valExpr: SCFWAE) extends SCFWAE
  case class SetBox(boxExpr: SCFWAE, valueExpr: SCFWAE) extends SCFWAE
  case class OpenBox(boxExpr: SCFWAE) extends SCFWAE

  implicit def idToSCFWAE(id: Symbol) = Id(id)
  implicit def numToSCFWAE(n: Int) = Num(n)

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

  // Env is now a mutual map!
  def interp(expr: SCFWAE, env: Env = Map(), store: Store = Map()): (Val, Store) = expr match {

    case Num(n) => (NumV(n), store)

    case Add(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, env, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, env, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 + n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    case Sub(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, env, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, env, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 - n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    case Mult(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, env, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, env, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 * n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    case Let(boundId, namedExpr, boundBody) => {
      val (namedVal, s1) = interp(namedExpr, env, store)
      val newLoc = nextLocation
      interp(boundBody, env + (boundId -> newLoc), s1 + (newLoc -> namedVal))
    }

    case Id(name) => (store(env(name)), store)

    case Fun(arg, body) => (Closure(arg, body, env), store)

    case If0(testExpr, thenExpr, elseExpr) => {
      val (testV, s1) = interp(testExpr, env, store)
      testV match {
        case NumV(n) => {
          if (n == 0) interp(thenExpr, env, s1)
          else interp(elseExpr, env, s1)
        }
        case _ => sys.error("can only test numbers, but got: " + testV)
      }
    }

    case App(funExpr, argExpr) => {
      val (funV, funStore) = interp(funExpr, env, store)
      val (argV, argStore) = interp(argExpr, env, funStore)
      funV match {
        case Closure(fParam, fBody, fEnv) => {
          val newLoc = nextLocation
          interp(fBody, fEnv + (fParam -> newLoc), argStore + (newLoc -> argV))
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }

    case Seqn(e1, e2) => {
      val (v1, s1) = interp(e1, env, store)
      interp(e2, env, s1)
    }

    case NewBox(boxExpr) => {
      val (boxV, boxStore) = interp(boxExpr, env, store)
      val newLoc = nextLocation
      (Box(newLoc), boxStore + (newLoc -> boxV))
    }

    case SetBox(boxExpr, valueExpr) => {
      val (boxV, s1) = interp(boxExpr, env, store)
      val (value, s2) = interp(valueExpr, env, s1)
      boxV match {
        case Box(loc) => (value, s2 + (loc -> value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
      }
    }

    case OpenBox(boxExpr) => {
      val (boxV, s1) = interp(boxExpr, env, store)
      boxV match {
        case Box(loc) => (s1(loc), s1)
        case _ => sys.error("can only open boxes, but got: " + boxV)
      }
    }

    case LetRec(boundId, namedExpr, boundBody) => {
      val newLoc = nextLocation
      // Environment is a mutual map that enables cyclic binding
      val recEnv = env + (boundId -> newLoc)
      val (namedVal, s1) = interp(namedExpr, recEnv, store)
      interp(boundBody, recEnv, s1 + (newLoc -> namedVal))
    }
  }

  // Test for factorial function
  val fact = LetRec('fact, Fun('n, If0('n, 1, Mult('n, App('fact, Sub('n, 1))))), 'fact)
  val (t1, _) = interp(App(fact, 5))
  assert(t1 == NumV(120))

  // Test of shadowing behavior of "letrec" and "let"
  val (t2, _) = interp(Let('x, 23, Let('x, Add('x, 42), 'x)))
  assert(t2 == NumV(65))

  val (t3, _) = interp(Let('x, 23, LetRec('y, Add('x, 42), 'y)))
  assert(t3 == NumV(65))
}
