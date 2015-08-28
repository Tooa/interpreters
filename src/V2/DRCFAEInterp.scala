package V2

/**
 * First-class interpreter with algebraic data types support.
 */
object DRCFAEInterp extends App {

  sealed abstract class DRCFAE
  case class Num(n: Int) extends DRCFAE
  case class Add(lhs: DRCFAE, rhs: DRCFAE) extends DRCFAE
  case class Sub(lhs: DRCFAE, rhs: DRCFAE) extends DRCFAE
  case class Mult(lhs: DRCFAE, rhs: DRCFAE) extends DRCFAE
  case class Id(name: Symbol) extends DRCFAE
  case class Fun(param: Symbol, body: DRCFAE) extends DRCFAE
  case class App(funExpr: DRCFAE, argExpr: DRCFAE) extends DRCFAE
  case class If0(test: DRCFAE, thenBody: DRCFAE, elseBody: DRCFAE) extends DRCFAE
  case class Rec(name: Symbol, namedExpr: DRCFAE, body: DRCFAE) extends DRCFAE
  case class Ctor(name: Symbol, args: List[DRCFAE]) extends DRCFAE
  case class Match(expr: DRCFAE, cases: List[(Symbol, List[Symbol], DRCFAE)]) extends DRCFAE

  implicit def symbolToExpr(symbol: Symbol) = Id(symbol)
  implicit def intToExpr(n: Int) = Num(n)

  // Allows us to write "Ctor('x)" instead of "Ctor('x, List())"
  object Ctor {
    def apply(name: Symbol): Ctor = Ctor(name, List())
  }

  // Values override toString for a bit nicer printing
  sealed abstract class Val
  case class NumV(n: Int) extends Val {
    override def toString = n.toString
  }
  case class CtorV(name: Symbol, args: List[Val]) extends Val
  case class ClosureV(param: Symbol, body: DRCFAE, env: Env) extends Val {
    override def toString = "(" + param.toString + ") => {" + body.toString + "}"
  }

  type Env = scala.collection.Map[Symbol, Val]

  def interp(expr: DRCFAE, env: Env = Map.empty): Val = expr match {
    case Num(n) => NumV(n)
    case Add(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => sys.error("can only add numbers, but got: " +(lhsV, rhsV))
      }
    case Sub(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
        case _ => sys.error("can only subtract numbers, but got: " +(lhsV, rhsV))
      }
    case Mult(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => sys.error("can only multiply numbers, but got: " +(lhsV, rhsV))
      }
    case Id(name) => env(name)
    case Fun(param, body) => ClosureV(param, body, env)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match {
        case ClosureV(funParam, funBody, funEnv) =>
          interp(funBody, funEnv + (funParam -> interp(argExpr, env)))
        case v => sys.error("can only apply functions, but got: " + v)
      }
    case If0(test, thenBody, elseBody) =>
      interp(test, env) match {
        case NumV(n) => interp(if (n == 0) thenBody else elseBody, env)
        case v => sys.error("can only test numbers, but got: " + v)
      }
    case Rec(name, namedExpr, body) =>
      val recEnv = collection.mutable.Map() ++ env
      recEnv += name -> interp(namedExpr, recEnv)
      interp(body, recEnv)
    case Ctor(name, args) =>
      CtorV(name, args.map(interp(_, env)))
    case Match(expr, cases) => interp(expr, env) match {
      case CtorV(name, argsV) => cases.collectFirst {
        //Partial function
        case(caseName, args, caseExpr) if(name == caseName) => interp(caseExpr, env ++ (args.zip(argsV)))
      } match {
        case Some(value) => value
        case _ => sys.error("No matching case found")
      }

      case _ => sys.error("Can only match ctor.")
    }
  }


  val trueV = Ctor('true)
  val falseV = Ctor('false)

  val zero = Ctor('zero)
  val one = Ctor('succ, List(zero))
  val two = Ctor('succ, List(one))

  val nil = Ctor('nil)
  val oneZeroList = Ctor('cons, List(one, Ctor('cons, List(zero, nil))))
  val countdownList = Ctor('cons, List(two, oneZeroList))

  val isNotZero = Fun('x, Match('x, List(
    ('zero, List(), falseV),
    ('succ, List('prev), trueV)
  )))

  val filterNatList = Rec('filter,
    Fun('list, Match('list, List(
      ('nil, List(), nil),
      ('cons, List('x, 'xs), Match(App(isNotZero, 'x), List(
        ('false, List(), App('filter, 'xs)),
        ('true, List(), Ctor('cons, List('x, App('filter, 'xs))))
      )
      )
        )))),
    App('filter, countdownList))

  println(interp(filterNatList))
}
