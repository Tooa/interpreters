// This file contains an interpreter for SCFWAE with simple mark&sweep garbage collection

/*
 * Based on the lecture notes for the "Programming Languages and Types"
 * course by Klaus Ostermann at the University of Marburg.
 */
package V5

object GC_SRCFWAEInterp extends App {
/* This model of garbage collection does not illustrate the difficulty
 * of memory management. In most languages, the size of the allocated
 * memory regions on the heap vary, and hence one needs an algorithm
 * to find a free and large-enough spot on the heap. There are various
 * algorithms and heuristics (best-fit, worst-fit, first-fit, ...) for
 * that purpose.
 *
 * There are also various alternative gc designs. Mark-and-sweep is a
 * non-moving algorithm, where reachable heap objects are never moved.
 * In contrast to that, copying gc algorithms move the reachable
 * objects to a different portion of the heap. One of the oldest
 * algorithms is the semi-space garbage collector, in particular with
 * the implementation purpose.

     http://www.cs.umd.edu/class/fall2002/cmsc631/cheney/cheney.html

 * Topic for class discussion: What are the pros and cons of moving
 * vs. non-moving gc?
 *
 * It can be shown empirically that most unreachable objects become
 * unreachable while they are still young. Generational gc algorithms
 * take this empirical fact into account and divide the objects into
 * generations, whereby the (small) youngest generation of objects is
 * garbage-collected more frequently.
 *
 * A typical problem of the simple gc algorithms we discussed is the
 * stop-the-world phenomenon: All execution has to be stopped during a
 * gc cycle. This issue is addressed by incremental or concurrent
 * garbage collectors. Incremental garbage collectors typically reduce
 * the total throughput but increase responsiveness and real-time
 * behavior.
 *
 * A completely different approach to memory management is _reference
 * counting_. In reference counting, each object on the heap (in our
 * case, each box) maintains a counter which says how many pointers
 * currently point to that object. The counter is adjusted whenever a
 * pointer variable is assigned to this object (incremented), or from
 * this object to another object (decremented). When the counter is 0,
 * the object can be reclaimed.
 *
 * The obvious disadvantage of reference counting is that it cannot
 * detect cycles on the heap. Hence reference counting algorithm must
 * be augmented with some means to detect cycles.
 *
 * Topic for class discussion: What are the pros and cons of reference
 * counting vs. tracing garbage collectors such as mark-and-sweep or
 * semi-space?
 */
    class MarkAndSweepStore(maxSize: Int) extends Store[Val] {

    val memory = new scala.collection.mutable.ArraySeq[Val](maxSize)
  
    var free : Int = maxSize
  
    var nextFreeAddr : Int = 0
  
    def malloc(stack: List[Env], v: Val) : (Location, Store[Val]) = {
      if (free <= 0) gc(stack)
      if (free <= 0) sys.error("out of memory")
  
      /* Here we find the next available location in memory via a while-
       * loop. In order to avoid maintaining a list of available spaces,
       *  let us assume that all boxes in SRCFWAE contain data
       *  (in constrast to null values).
       *
       * If we ensure the invariant that the variable `free` has always
       * the number of free memory space, then the following loop will
       * always halt. The nontermination situation will generate an out-
       * of-memory error and the program will abort.
       */
  
      while (memory(nextFreeAddr) != null) {
        nextFreeAddr += 1
        if (nextFreeAddr == maxSize) nextFreeAddr = 0
      }
  
      free -= 1
      update(nextFreeAddr, v)
      (nextFreeAddr, this)
    }
  
    def update(index: Location, v: Val) : Store[Val] = { 
      memory.update(index, v)
      this 
    }
  
    def lookup(index: Int) = memory(index)
    
    def free(index: Int) = {
      free += 1
      memory(index) = null
      this
    }
  
    def allAddrInVal(v: Val) : Set[Int] = v match {
      case Box(a) => Set(a)
      case NumV(_) => Set()
      case Closure(f, body, env) => env.values.toSet
    }
  
    def mark(seed: Set[Int]) : Unit = {
      for (i <- seed)
        memory(i).marked = true
      val allAddresses = seed flatMap (i => allAddrInVal(memory(i)))
      val newAddresses = allAddresses filter (i => !memory(i).marked) 
      if(!newAddresses.isEmpty)
        mark(newAddresses)
    }
    /* 
     * What graph algorithm underlies the mark step as implemented here?
     * What potential problem it could cause in a "real" interpreter?
     */
    
    def sweep() : Unit = {
      for (i <- memory.indices) {
        val v = memory(i)
        if (v == null) {
          /* No work needed on an empty memory cell */
        }
        else if (v.marked) {
          /* Reset `marked` flag for the next gc */
          v.marked = false
        }
        else {
          free += 1
          memory(i) = null
        }
      }
    }
  
    def gc(stack: List[Env]) : Unit = {
      println("\nSTARTING GC\nSTACK = " + stack + "\nSTORE = " + memory)
      val seed: Set[Int] = stack.map(env => env.values.toSet).fold(Set.empty)(_ union _)
      mark(seed)
      sweep()
      println("GC COMPLETE\nSTORE = " + memory +
              "\nNUMBER OF FREE SLOTS = " + free)
    }
  }
  
  
/* This model of garbage collection does not illustrate the difficulty
 * of memory management. In most languages, the size of the allocated
 * memory regions on the heap vary, and hence one needs an algorithm
 * to find a free and large-enough spot on the heap. There are various
 * algorithms and heuristics (best-fit, worst-fit, first-fit, ...) for
 * that purpose.
 *
 * There are also various alternative gc designs. Mark-and-sweep is a
 * non-moving algorithm, where reachable heap objects are never moved.
 * In contrast to that, copying gc algorithms move the reachable
 * objects to a different portion of the heap. One of the oldest
 * algorithms is the semi-space garbage collector, in particular with
 * the implementation purpose.

     http://www.cs.umd.edu/class/fall2002/cmsc631/cheney/cheney.html

 * Topic for class discussion: What are the pros and cons of moving
 * vs. non-moving gc?
 *
 * It can be shown empirically that most unreachable objects become
 * unreachable while they are still young. Generational gc algorithms
 * take this empirical fact into account and divide the objects into
 * generations, whereby the (small) youngest generation of objects is
 * garbage-collected more frequently.
 *
 * A typical problem of the simple gc algorithms we discussed is the
 * stop-the-world phenomenon: All execution has to be stopped during a
 * gc cycle. This issue is addressed by incremental or concurrent
 * garbage collectors. Incremental garbage collectors typically reduce
 * the total throughput but increase responsiveness and real-time
 * behavior.
 *
 * A completely different approach to memory management is _reference
 * counting_. In reference counting, each object on the heap (in our
 * case, each box) maintains a counter which says how many pointers
 * currently point to that object. The counter is adjusted whenever a
 * pointer variable is assigned to this object (incremented), or from
 * this object to another object (decremented). When the counter is 0,
 * the object can be reclaimed.
 *
 * The obvious disadvantage of reference counting is that it cannot
 * detect cycles on the heap. Hence reference counting algorithm must
 * be augmented with some means to detect cycles.
 *
 * Topic for class discussion: What are the pros and cons of reference
 * counting vs. tracing garbage collectors such as mark-and-sweep or
 * semi-space?
 */
  
  
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
  
/* We equip our values with a mutable flag that is useful for
 * mark-and-sweep garbage collection. In real systems it is
 * implemented as a bit flag, or, if the so-called "tri-color
 * algorithm" is used, with two bit flags.
 */
  sealed abstract class Val(var marked: Boolean = false)
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
    store: Store[Val] = new MarkAndSweepStore(100)): (Val, Store[Val]) = expr match {

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
      val extStack = Map(boundId -> newLoc) :: stack
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
  assert(ts5.lookup(1) == NumV(6))

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
    for (i <- 1 to n)
      v = Seqn(NewBox(i), v)
    v
  }
  
  override def main(args: Array[String]) {
    val iterations = args(0).toInt
    val storeSize = args(1).toInt
    val store = new MarkAndSweepStore(storeSize)
    val (v, s) = (interp(whatDoesThisDo(iterations), store = store))
    println(v)
    println(s)
    println(s"all ok, final store size is ${storeSize - store.free}")
  }
}
