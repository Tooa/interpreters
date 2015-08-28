// This file contains an interpreter for SCFWAE with recursive first-class functions, conditionals, mutable boxes, variables and sequencing.
package V5


/**
 * Reference Counting is a technique for automatic memory management.
 * In general, we keep track of the number of references for each object.
 * If the number reaches 0, the object can be released.
 *
 * Releasing a single object can free other objects e.g closures on the store
 * will enref all referenced values in the captures env.
 *
 * + re-use freed cells immedialtly
 * + incremental: deallocate as soon as object is unreferenced
 *
 * However, reference counting has an important limitation. It fails on
 * cyclic data structures. Consider the following example of a circular
 * linked list:
 *
 * Before:
 *
 *
 * Head -> elem1 (refCount 2) -> elem2 (refCount 1) -> elem3 (refCount 1) -
 *         ^^                                                             |
 *         |                                                              |
 *         ----------------------------------------------------------------
 *
 * Consider what happens when we assign the value null to the head variable:
 *
 *
 * Head -> null    elem1 (refCount 1) -> elem2 (refCount 1) -> elem3 (refCount 1) ---
 *                  ^^                                                              |
 *                  |                                                               |
 *                  ----------------------------------------------------------------
 *
 * The reference counts on all list elements are non-zero. Therefre, they are not considered
 * to be garbage by a reference counting collector. On the other hand, no external reference
 * to the linked-list-elements remain. Therefore, the list elements are indeed garbage.
 *
 */
object RefCount_SRCFWAEInterp extends App {
  sealed abstract class Expr
  case class Num(n: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr
  case class With(name: Symbol, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: Symbol) extends Expr
  case class If0(test: Expr, posBody: Expr, negBody: Expr) extends Expr
  case class Fun(param: Symbol, body: Expr) extends Expr
  case class Rec(name: Symbol, namedExpr: Expr, body: Expr) extends Expr
  case class App(funExpr: Expr, argExpr: Expr) extends Expr
  case class Seqn(e1: Expr, e2: Expr) extends Expr
  case class SetId(id: Symbol, valueExpr: Expr) extends Expr
  case class NewBox(valExpr: Expr) extends Expr
  case class SetBox(boxExpr: Expr, valueExpr: Expr) extends Expr
  case class OpenBox(boxExpr: Expr) extends Expr

  type Location = Int
  type Env = Map[Symbol, Location]

  sealed abstract class Val {
    var refCount: Int = 1
  }
  case class NumV(n: Int) extends Val
  case class Closure(param: Symbol, body: Expr, env: Env) extends Val
  case class Box(location: Location) extends Val

  
  /*
   * Dereference value.
   * If there was only one reference,
   *  recursively dereference nested values.
   */
  def deref(v: Val, store: Store[Val]): Store[Val] = {
    v.refCount = v.refCount - 1
    if (v.refCount <= 0)
      destroy(v, store)
    else
      store
  }
  def deref(loc: Location, store: Store[Val]): Store[Val] = {
    val v = store.lookup(loc)
    val st = deref(v, store)
    if (v.refCount <= 0)
      st.free(loc)
    else
      st
  }
    
  
  /*
   * Mark a new reference to value.
   */
  def enref(v: Val): Val = {
    v.refCount = v.refCount + 1
    v
  }

  /*
   * Dereference nested locations of a value.
   */
  def destroy(v: Val, store: Store[Val]): Store[Val] = {
    v match {
      case NumV(_) => store
      case Box(loc) => deref(loc, store)
      case Closure(x, body, env) => 
        env.values.foldRight(store)((loc, store2) => deref(loc, store2))
    }
  }

  def interp(
    expr: Expr,
    stack: List[Env] = List(Map()),
    store: Store[Val] = new NoGCStore[Val](100)): (Val, Store[Val]) = {
    
//    println(store.size)
    
    expr match {

    case Num(n) => (NumV(n), store)

    // deref the intermediate values lhsv and rhsv
    case Add(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) =>
              val s3 = deref(lhsv, deref(rhsv, s2))
              (NumV(n1 + n2), s3)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }
    case Mult(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) =>
              // After evaluating this expression, no one else will need it, so we deref it
              val s3 = deref(lhsv, deref(rhsv, s2))
              (NumV(n1 * n2), s3)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    // refCount of namedVal does not change since we deref the intermediate result
    // and immediately enref the value of newLoc
    case With(boundId, namedExpr, boundBody) => {
      // We interpreted namedExpr to namedVal so probably no one will use it again so deref it
      // In general, we always deref intermediate results
      // deref(namedVal, ...)
      // We allocate new memory and add a pointer to namedVal so enref it
      val (namedVal, s1) = interp(namedExpr, stack, store)
      val (newLoc, s2) = s1.malloc(stack, namedVal)
      //  enref(namedVal)
      //  val s3 = deref(namedVal, s2)
      val (v, s3) = interp(boundBody, stack.head + (boundId -> newLoc) :: stack.tail, s2)
      // After we interpreted the boundBody with the namedExpr binding, We can deref it
      // indirect with its location (newLoc).
      val s4 = deref(newLoc, s3)
      (v, s4)
    }

    case Id(name) => {
      val v = store.lookup(stack.head(name))
      enref(v)
      (v, store)
    }

    case Fun(arg, body) => {
      val env = stack.head
      env.map(kv => enref(store.lookup(kv._2)))
      
      (Closure(arg, body, env), store)
    }

    case If0(testExpr, thenExpr, elseExpr) => {
      val (testV, s1) = interp(testExpr, stack, store)
      testV match {
        case NumV(n) => {
          val s2 = deref(testV, s1)
          if (n == 0) interp(thenExpr, stack, s2)
          else interp(elseExpr, stack, s2)
        }
        case _ => sys.error("can only test numbers, but got: " + testV)
      }
    }
    
    /**
     * In our stateful language, we do not require mutation from the
     * host language to implement cyclic environments.
     */
    case Rec(boundId, namedExpr, boundBody) => {
      val (newLoc,s2) = store.malloc(stack, NumV(0))
      val extStack = Map(boundId -> newLoc) :: stack
      val (namedVal, bodyStore) = interp(namedExpr, extStack, s2)

      // deref and enref because of overwrite in `update`
      val st = deref(bodyStore.lookup(newLoc), bodyStore)
      enref(namedVal)
      val st2 = st.update(newLoc, namedVal)
      
      interp(boundBody, extStack, st2)
    }

    case App(funExpr, argExpr) => {
      val (funV, funStore) = interp(funExpr, stack, store)
      val funStore2 = deref(funV, funStore)
      val (argV, argStore) = interp(argExpr, stack, funStore2)
      funV match {
        case Closure(fParam, fBody, fEnv) => {
          val (newLoc, resStore) = argStore.malloc(stack, argV)
//          enref(argV)
//          val resStore2 = deref(argV, resStore)
          val (v, resStore2) = interp(fBody, List(fEnv + (fParam -> newLoc)), resStore)
          val resStore3 = deref(newLoc, resStore2)
          (v, resStore3)
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }

    case Seqn(e1, e2) => {
      val (v1, s1) = interp(e1, stack, store)
      val s2 = deref(v1, s1)
      interp(e2, stack, s2)
    }

    case NewBox(boxExpr) => {
      val (boxV, boxStore) = interp(boxExpr, stack, store)
      val (newLoc, resStore) = boxStore.malloc(stack, boxV)
      (Box(newLoc), resStore)
    }

    case SetBox(boxExpr, valueExpr) => {
      val (boxV, s1) = interp(boxExpr, stack, store)
      val (value, s2) = interp(valueExpr, stack, s1)
      boxV match {
        case Box(loc) =>
          //val s4 = deref(loc, s2)
          val old = s2.lookup(loc)
          // We deref the old value and assume it will not be needed anymore
          val s3 = deref(old, s2)
          // we enref value, because it will be bound to the old location so a new handle is assigned
          enref(value)
          val s4 = deref(boxV, s3)
          (value, s4.update(loc, value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
      }
    }

    case OpenBox(boxExpr) => {
      val (boxV, s1) = interp(boxExpr, stack, store)
      boxV match {
        case Box(loc) => 
          val v = s1.lookup(loc)
          // enref value because we are going to return a handle on this
          enref(v)
          // deref box expression because we assume it's no longer needed
          val s2 = deref(boxV, s1)
          (v, s2)
        case _ => sys.error("can only open boxes, but got: " + boxV)
      }
    }

    case SetId(id, valExpr) => {
      val (value, s1) = interp(valExpr, stack, store)
      val loc = stack.head(id)
      enref(value)
      (value, deref(loc, s1).update(loc, value))
    }
  }
  }
  
  // Some assertions on the interpreter
  import scala.language.implicitConversions

  implicit def idToSCFWAE(id: Symbol) = Id(id)
  implicit def numToSCFWAE(n: Int) = Num(n)

  val (tv1, _) = interp(With('a, NewBox(1), OpenBox('a)))
  assert(tv1 == NumV(1))

  val (tv2, _) = interp(
    With('a, NewBox(1),
      With('f, Fun('x, Add('x, OpenBox('a))),
        Seqn(SetBox('a, 2), App('f, 5)))))
  println(tv2)
  assert(tv2 == NumV(7))

  val (tv3, _) = interp(
    With('switch, NewBox(0),
      With('toggle,
        Fun('dummy,
          If0(OpenBox('switch),
            Seqn(SetBox('switch, 1), 1),
            Seqn(SetBox('switch, 0), 0))),
        Add(App('toggle, 42), App('toggle, 42)))))
  assert(tv3 == NumV(1))

  val (tv4, _) = interp(
    With('switch, 0,
      With('toggle,
        Fun('dummy,
          If0('switch,
            Seqn(SetId('switch, 1), 1),
            Seqn(SetId('switch, 0), 0))),
        Add(App('toggle, 42), App('toggle, 42)))))
  assert(tv4 == NumV(1))

  val (tv5, ts5) = interp(
    App(Fun('b1, App(Fun('b2, Seqn(SetBox('b1, 6), OpenBox('b2))), NewBox(7))),
      NewBox(5)))
  assert(tv5 == NumV(7))
  assert(ts5.lookup(0) == NumV(6))

  val (tv6, _) = interp(
    With('b, 0,
      If0(Seqn(SetId('b, 5), 'b),
        1,
        'b)))
  assert(tv6 == NumV(5))

  val (tv7, _) = interp(With('b, 4, Add('b, Seqn(SetId('b, 5), 'b))))
  assert(tv7 == NumV(9))
  
  assert(interp(
    Rec('fact, Fun('n, If0('n, 1, Mult('n, App('fact, Add('n, -1))))),
      App('fact, 5)))._1 == NumV(120))
  assert(interp(With('x, 3, Fun('y, Add('x, 'y)))) == (Closure('y, Add('x, 'y), Map('x -> 1)), Map(1 -> NumV(3))))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5))))._1 == NumV(11))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)), 'inc))._1 == Closure('x, Add('x, 1), Map()))
  assert(interp(With('x, 3, App(Fun('y, Add('x, 'y)), 4)))._1 == NumV(7))
  
  
  def whatDoesThisDo(n: Int) : Expr = {
    var v: Expr = Num(17)
    for (i <- 0 to n)
      v = Seqn(NewBox(i), v)
    v
  }
  
  /*override def main(args: Array[String]) {
    val iterations = args(0).toInt
    val storeSize = args(1).toInt
    val store = new NoGCStore[Val](storeSize)
    interp(whatDoesThisDo(iterations), store = store)
    println(s"all ok, final store size is ${store.nextFreeAddr - store.freed.size}")
  }*/
}
