package lecture2

/**
 * Taxonomy of Functions
 * ======================
 *
 * First-class Functions
 *    Functions are values that can be passed around, stored in
 *    data structures and created at run time. Function applications
 *    are _expressions_!
 *
 * Higher-order Functions
 *    Functions that return and/or take other functions as parameters.
 *
 * First-order Functions
 *    Functions that neither return nor take other functions as parameters.
 *
 *
 * Static vs. Dynamic scoping
 * ==========================
 *
 * Static Scoping
 *    The scope of a name binding is determined at compile time.
 *
 *    val funEnv = Map(param -> interp(arg, funDefs, env))
 *
 *
 * Dynamic Scoping
 *    The scope of a name binding is determined by the execution context
 *    at run time. The interpreter will walk up the symbol table to find
 *    the instance of the variable.
 *
 *    val funEnv = env + Map(param -> interp(arg, funDefs, env))
 *
 * Example
 *
 *    const int b = 5;
 *    int foo()
 *    {
 *       int a = b + 5;
 *       return a;
 *    }
 *
 *    int bar()
 *    {
 *       int b = 2;
 *       //Function application opens a new scope (fresh/empty env) for static scoping
 *       return foo();
 *    }
 *
 *    int main()
 *    {
 *       //Dynamic Scoping
 *       foo(); // returns 10
 *       bar(); // returns 7
 *
 *       //Static Scoping
 *       foo(); // returns 10
 *       bar(); // returns 10
 *       return 0;
 *    }
 *
 */

/**
 * Application Example:
 *
 * interp(With('n, 5, App('f, 10)), Map('f -> FunDef('x, 'n)))
 *
 * F1WAEImmediateSubstInterp
 *    Yields unbound identifier n, because the substitution is only
 *    applied to the App not the FunDef
 *
 * F1WAEStaticInterp
 *    Yields unbound identifier n, because the function call receives
 *    a fresh environment and n is _not_ bound in the function.
 *
 *    val funEnv = Map(param -> interp(arg, funDefs, env))
 *
 * F1WAEDynamicInterp
 *    Evaluetes to 5, because n is bound in the environment given to
 *    the function application.
 *
 *    val funEnv = env + Map(param -> interp(arg, funDefs, env))
 */


/**
 * A major difficulty of substitution is the number of times we traverse
 * the source program. Substitution traverses everything for example unvisited
 * branches of conditionals. We come over this problem by introducing the
 * environment concept that enables fast identifier lookups.
 */
object F1WAE extends App {

}
