package V2

/**
 * A major difficulty of substitution is the number of times we traverse
 * the source program. Substitution traverses everything - e.g unvisited
 * branches of conditionals. We come over this problem by introducing the
 * environment concept that enables fast identifier lookups. Note, we still
 * have only integers as values (First-Order language).
 *
 * In addition we will use dynamic scoping, where the scope binding for
 * an identifier is determined the by execution context at runtime. So
 * a construct like the following will be valid:
 *
 *    With(n, 5, f(10)) where f(x) = { n }
 *
 * We will implement dynamic scoping by extending current environment for every function
 * application with the function argument bindings.
 */
object F1WAEDynamicInterp extends App {

}
