package V2


/**
 * In practice dynamic scoping is considered to be bad, because it gives us
 * hard times in identifying the current identifier binding. This is due to
 * the fact, that the binding is determined at runtime.
 *
 * To overcome this issue we implement static scoping. For First-Order languages
 * this can be done by passing a fresh clean environment to the function application.
 * Note, that this will not work out for first-class languages, where functions can
 * appear in With/Let-Bindings.
 */
object F1WAEStaticnterp extends App {

}
