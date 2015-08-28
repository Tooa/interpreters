// This file contains an interpreter for SCFWAE with recursive first-class functions, conditionals, mutable boxes, variables and sequencing.

/*
 * Based on the lecture notes for the "Programming Languages and Types"
 * course by Klaus Ostermann at the University of Marburg.
 */
package V5

/* To be able to experiment with different store and gc designs, we
 * create an interface for stores. The stack parameter in malloc is
 * needed during gc to determine the root nodes from which the
 * algorithms can start.
 */
trait Store[Val] {
  def malloc(stack: List[Map[Symbol, Int]], v: Val) : (Int, Store[Val])
  def update(index: Int, v: Val) : Store[Val]
  def lookup(index: Int) : Val
  def free(index: Int) : Store[Val]
}

  /* Here is one implementation of the Store interface that does not
   * perform gc. It just runs out of memory once the store is full.
   */
class NoGCStore[Val](var maxSize: Int) extends Store[Val] {
    val memory = new scala.collection.mutable.ArraySeq[Val](maxSize)
    var freed = Set[Int]()
    
    var nextFreeAddr : Int = 0
  
    def malloc(stack: List[Map[Symbol, Int]], v: Val) = {
      if (!freed.isEmpty) {
        val next = freed.head
        freed -= next
        update(next, v)
        (next, this)
      }
      else {
        val x = nextFreeAddr
        if (x > maxSize) sys.error("out of memory")
        nextFreeAddr += 1
        update(x, v)
        (x, this)
      }
    }
  
    def update(index: Int, v: Val) = {
      memory.update(index, v)
      this
    }
    
    def free(index: Int) = {
      freed += index
      this
    }
  
    def lookup(index: Int) = memory(index)

    override def toString() = memory.toString
  }


object SRCFWAEInterp extends App {
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
  
  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: Symbol, body: Expr, env: Env) extends Val
  case class Box(location: Location) extends Val
  
  /* In our interpreter, the stack of environments is only implicitly
   * available on the stack of the meta-language. To reify the call-
   * stack we need to make it explicit. We do so by constructing the
   * stack explicitly and passing it as parameter. The first element
   * of the stack is the current environment; the rest is only needed
   * for gc.
   */
  def interp(
    expr: Expr,
    stack: List[Env] = List(Map()),
    store: Store[Val] = new NoGCStore[Val](100)): (Val, Store[Val]) = expr match {

    case Num(n) => (NumV(n), store)

    case Add(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
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
    case Mult(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
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

    case With(boundId, namedExpr, boundBody) => {
      val (namedVal, s1) = interp(namedExpr, stack, store)
      val (newLoc, s2) = s1.malloc(stack, namedVal)
      interp(boundBody, stack.head + (boundId -> newLoc) :: stack.tail, s2)
    }

    case Id(name) => (store.lookup(stack.head(name)), store)

    case Fun(arg, body) => (Closure(arg, body, stack.head), store)

    case If0(testExpr, thenExpr, elseExpr) => {
      val (testV, s1) = interp(testExpr, stack, store)
      testV match {
        case NumV(n) => {
          if (n == 0) interp(thenExpr, stack, s1)
          else interp(elseExpr, stack, s1)
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
      val extStack = stack.head + (boundId -> newLoc) :: stack
      val (namedVal, bodyStore) = interp(namedExpr, extStack, store)
      interp(boundBody, extStack, bodyStore.update(newLoc, namedVal))
    }

    case App(funExpr, argExpr) => {
      val (funV, funStore) = interp(funExpr, stack, store)
      val (argV, argStore) = interp(argExpr, stack, funStore)
      funV match {
        case Closure(fParam, fBody, fEnv) => {
          val (newLoc, resStore) = argStore.malloc(stack, argV)
          interp(fBody, fEnv + (fParam -> newLoc) :: stack, resStore)
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }

    case Seqn(e1, e2) => {
      val (v1, s1) = interp(e1, stack, store)
      interp(e2, stack, s1)
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
        case Box(loc) => (value, s2.update(loc, value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
      }
    }

    case OpenBox(boxExpr) => {
      val (boxV, s1) = interp(boxExpr, stack, store)
      boxV match {
        case Box(loc) => (s1.lookup(loc), s1)
        case _ => sys.error("can only open boxes, but got: " + boxV)
      }
    }

    case SetId(id, valExpr) => {
      val (value, s1) = interp(valExpr, stack, store)
      (value, s1.update(stack.head(id), value))
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
  println(interp(With('x, 3, Fun('y, Add('x, 'y)))))
  assert(interp(With('x, 3, Fun('y, Add('x, 'y)))) == (Closure('y, Add('x, 'y), Map('x -> 0)), Map(0 -> NumV(3))))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)),
      Add(App('inc, 4), App('inc, 5))))._1 == NumV(11))
  assert(interp(
    With('inc, Fun('x, Add('x, 1)), 'inc))._1 == Closure('x, Add('x, 1), Map()))
  assert(interp(With('x, 3, App(Fun('y, Add('x, 'y)), 4)))._1 == NumV(7))

  def whatDoesThisDo(n: Int) : Expr = {
    var v: Expr = Num(17)
    for (i <- 1 to n)
      v = Seqn(NewBox(i), v)
    v
  }
  
  /*override def main(args: Array[String]) {
    val iterations = args(0).toInt
    val storeSize = args(1).toInt
    val store = new NoGCStore[Val](storeSize)
    interp(whatDoesThisDo(iterations), List(Map()), store)
    println(s"all ok, final store size is ${store.nextFreeAddr - store.freed.size}")
  }*/
}
