package lecture1

import scala.language.implicitConversions

/**
 * Host language:
 *    Language in which the interpreter is implemented.
 *
 * Interpreted language:
 *    The language that the interpreter evaluates.
 *
 *  Meta-Interpreter
 *    Host language == Interpreted Language
 *
 *  Substitution:
 *    To substitute identifier i in e with expression v, replace
 *    all _free_ instances of i in e with v.
 */

sealed abstract class WAE

case class Num(n: Int) extends WAE
case class Add(lhs: WAE, rhs: WAE) extends WAE
case class Sub(lhs: WAE, rhs: WAE) extends WAE
case class Let(name: Symbol, namedExpr: WAE, body: WAE) extends WAE
case class Id(name: Symbol) extends WAE

object WAEInterp extends App {

  /**
   * This method will substitute/replace all free instances of
   * <code>substId</code> in <code>expr</code> with <code>value</code>.
   */
  def subst(expr: WAE, substId: Symbol, value: WAE): WAE = expr match {
    case Num(n) => Num(n) //same as expr
    case Add(lhs, rhs) => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      // Shadowing: Occurrence of inner Let expression
      // Let('x, 1, Let('x, 2, Add('x, 'x))
      if(boundId == substId) {
        // There is another binding of substId, so stop replacing
        Let(boundId, substNamedExpr, boundExpr)
      } else {
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))
      }
    case Id(name) =>
      // In case, the substId matches with the found Id replace it with the value
      if(substId == name) value else Id(name)
  }

  /**
   * This substitution strategy substitutes an expression as soon as possible.
   * This also forces the evaluation of expressions that may not be necessary
   * at run time.
   */
  def eagerCalc(expr: WAE): Int = expr match {
    case Num(n) => n
    case Add(lhs, rhs) => eagerCalc(lhs) + eagerCalc(rhs)
    case Sub(lhs, rhs) => eagerCalc(lhs) - eagerCalc(rhs)
    case Let(boundId, namedExpr, boundExpr) =>
      eagerCalc(subst(boundExpr, boundId, Num(eagerCalc(namedExpr))))
    case Id(name) => sys.error("Found unbound id " + name)
  }

  /**
   * This substitution strategy delays the substitution of an expression
   * until its value is needed.
   */
  def lazyCalc(expr: WAE): Int = expr match {
    case Num(n) => n
    case Add(lhs, rhs) => eagerCalc(lhs) + eagerCalc(rhs)
    case Sub(lhs, rhs) => eagerCalc(lhs) - eagerCalc(rhs)
    case Let(boundId, namedExpr, boundExpr) =>
      lazyCalc(subst(boundExpr, boundId, namedExpr))
    case Id(name) => sys.error("Found unbound id " + name)
  }

  // Assertions on the interpreter
  implicit def symbolToWAE(symbol: Symbol) = Id(symbol)
  implicit def intToWAE(n: Int) = Num(n)

  assert(eagerCalc(Let('x, Add(5, 5), Add('x, 'x))) == 20)
  // Shadowing behaviour
  assert(eagerCalc(Let('x, 1, Let('x, 2, Add('x, 'x)))) == 4)
  try {
    eagerCalc(Let('x, Add(3, 'z), Let('y, 100, 'y)))
    assert(false)
  } catch {
    case e: Exception => assert(true)
  }
  assert(lazyCalc(Let('x, Add(5, 5), Add('x, 'x))) == 20)
  assert(eagerCalc(Let('x, 1, Let('x, 2, Add('x, 'x)))) == 4)
  assert(lazyCalc(Let('x, Add(3, 'z), Let('y, 100, 'y))) == 100)
}
