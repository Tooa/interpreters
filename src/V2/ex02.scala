/*
 * Exercise 02, April 24
 */

object ex02 {

/*
 * Task 1: Arithmetics
 *
 * 1) Write a recursive function 'sum' that sums up the elements of a list of numbers.
 */

def sum(l: List[Int]): Int = ???

/*
 * 2) Write a recursive function 'prod' that builds the product of the elements in list of numbers.
 */

def prod(l: List[Int]): Int = ???

/*
 * 3) Can you recognize similarities between 'sum' and 'prod'? Write a higher-order function 'fold' that
 *    abstracts over these similarities. Reimplement 'sum' and 'prod' in terms of 'fold'.
 */

// def fold(...): ... =
//   ...

/*
 * Task 2: Trees
 */

abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

/*
 * 1) Write a recursive function 'sum' that sums up the elements of a tree of numbers.
 */

def sumTree(l: Tree[Int]): Int = ???

/*
 * 2) Write a recursive function 'collectTree' that collects all elements stored in the leaves of a tree.
 */

def collectTree[A](l: Tree[A]): Set[A] = ???

/*
 * 3) Can you recognize similarities between 'sumTree' and 'collectTree'? Write a higher-order function 'foldTree' that
 *    abstracts over these similarities. Reimplement 'sumTree' and 'collectTree' in terms of 'foldTree'.
 */

// def foldTree(...): ... =
//   ...


/*
 * Task 3: Programs
 */

abstract class F1WAE
case class Num(n: Int) extends F1WAE
case class Add(lhs: F1WAE, rhs: F1WAE) extends F1WAE
case class Sub(lhs: F1WAE, rhs: F1WAE) extends F1WAE
case class Let(name: Symbol, namedExpr: F1WAE, body: F1WAE) extends F1WAE
case class Id(name: Symbol) extends F1WAE
case class App(funName: Symbol, arg: F1WAE) extends F1WAE

/*
 * 1) Write a function 'progSize' that counts the number of AST nodes in a program.
 */

def progSize(p: F1WAE): Int = ???

/*
 * 2) Write a function 'freevars' that collects the free (unbound) variables of a program.
 */

def freevars(p: F1WAE): Set[Symbol] = ???

/*
 * 3) Can you recognize similarities between 'progSize' and 'freevars'? Write a higher-order function 'foldF1WAE' that
 *    abstracts over these similarities. Reimplement 'progSize' and 'freevars' in terms of 'foldF1WAE'
 */

// def foldF1WAE(...): ... =
//   ...
}
