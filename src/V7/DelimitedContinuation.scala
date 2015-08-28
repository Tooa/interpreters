package V7

import scala.util.continuations._

/**
 * Compared to CPS transformation represents the concept of
 * first-class continuation (delimited continuation) a
 * technique that does not require to transform the program
 * a priori. Reifications of rest-of- computation can be done
 * on-the-fly.
 *
 * To compile the code the  -P:continuations:enable flag needs
 * to be enabled e.g
 *
 * scalac -P:continuations:enable DelimitedContinuation.scala
 * scala Main
 *
 */
object DelimitedContinuation extends App {
  def foo() = { 1 + shift{k: (Int=>Int) => k(k(k(7)))}}
  def bar() = {foo() * 2}
  def baz() = { reset{bar() + 10} }
  println(baz())
}
