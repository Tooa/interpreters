package V6

/**
 * First-order classes interpreter. This means we have predefined classes
 * with methods and field. We don't need state, because we model immutable
 * objects.
 *
 * Left out (for now) -> inheritance, mutation of fields
 *
 * "New" evaluates to Object and binds the fields of a class
 * with concrete values. The order does matter here ;)
 *
 * 'Box -> Class(List('v), Map('get -> Method(Nil, FAcc(Id('this), 'v))))
 *
 * New('Box, List(fE))
 *
 * Binds 'v with fE
 */
object OOInterp extends App {
  
  sealed abstract class Expr
  case class Id(x: Symbol) extends Expr
  case class New(cl: Symbol, fvals: List[Expr]) extends Expr
  case class FAcc(o: Expr, field: Symbol) extends Expr
  case class Invoke(o: Expr, method: Symbol, args: List[Expr]) extends Expr
  case class Let(boundId: Symbol, namedExpr: Expr, boundBody: Expr) extends Expr
  
  sealed abstract class Value
  case class Object(cl: Symbol, fvals: List[Value]) extends Value
  
  case class Class(fields: List[Symbol], methods: Map[Symbol, Method])
  case class Method(params: List[Symbol], body: Expr)
  
  type Env = Map[Symbol, Value]
  type ClassTable = Map[Symbol, Class]

  def interp(e: Expr, env: Env, ct: ClassTable): Value = e match {
    case Id(x) => env(x)

    case Let(boundId, namedExpr, boundBody) =>
      val namedVal = interp(namedExpr, env, ct)
      interp(boundBody, env + (boundId -> namedVal), ct)

    case New(cl, fexps) =>
      val fvals = fexps map (e => interp(e, env, ct))
      Object(cl, fvals)

    // FAcc(Id('this), 'v)
    // Access on object this the field 'v -> field Access
    case FAcc(oexp, field) =>
      val Object(cl, fvals) = interp(oexp, env, ct)
      val Class(fnames, methods) = ct(cl)
      val findex = fnames.indexOf(field)
      fvals(findex)
    
    case Invoke(oexp, method, args) =>
      val rcv@Object(cl, fvals) = interp(oexp, env, ct)
      val Class(fnames, methods) = ct(cl)
      val Method(params, body) = methods(method)
      
      val argVals = args map (e => interp(e, env, ct))
      val paramEnv = Map() ++ (params zip argVals)
      val envInvoke = paramEnv + ('this -> rcv)
      
      interp(body, envInvoke, ct)
  }
  
  val testclasses = Map(
      'True -> Class(Nil, Map('and -> Method(List('other), Id('other)))),
      'False -> Class(Nil, Map('and -> Method(List('other), Id('this)))),
      'Box -> Class(List('v), Map('get -> Method(Nil, FAcc(Id('this), 'v)))),
      'Let -> Class(Nil, Map('get -> Method(List('other), Let('x, Id('other), Id('x)))))
  )
      
  val tE = New('True, Nil)
  val tV = Object('True, Nil)
  val fE = New('False, Nil)
  val fV = Object('False, Nil)
  val lE = New('Let, Nil)

  def eval(e: Expr) = interp(e, Map(), testclasses)
  
  assert(eval(Invoke(New('Box, List(fE)), 'get, Nil)) == fV)
  assert(eval(Invoke(tE, 'and, List(tE))) == tV)
  assert(eval(Invoke(fE, 'and, List(tE))) == fV)

  assert(eval(Invoke(lE, 'get, List(fE))) == fV)
}