package oo

/**
 * OO Interpreter with inheritance. The case class Class is extended by
 * a field containing its super class. For method calls and field access
 * one has to lookup the path up to the super class and return the first
 * catch.
 */
object OOWithInheritanceInterp extends App {
  sealed abstract class Expr
  case class New(className: Symbol, fields: List[Expr]) extends Expr
  case class FAcc(objExpr: Expr, fieldName: Symbol) extends Expr
  case class Invoke(objExpr: Expr, methodName: Symbol, args: List[Expr]) extends Expr
  case class Id(id: Symbol) extends Expr

  case class Class(
    // Extended super class
    superClass: Symbol,
    fields: List[Symbol],
    methods: Map[Symbol, Method])

  case class Method(params: List[Symbol], body: Expr)

  sealed abstract class Value
  case class Object(className: Symbol, fields: List[Value]) extends Value

  type Env = Map[Symbol, Value]

  /**
   * Returns all fields in the path from the root to className in the
   * inheritance tree
   */
  def lookupField(
    fieldName: Symbol,
    className: Symbol,
    fieldVals: List[Value],
    classes: Map[Symbol, Class]): Value = className match {
    case 'Object => sys.error("Unknown field %s".format(fieldName))
    case _ => {
      val clazz = classes.getOrElse(className, sys.error("Unknown class %s".format(className)))
      val index = clazz.fields.indexOf(fieldName)
      if (index >= 0)
        fieldVals(index)
      else
        lookupField(fieldName, clazz.superClass, fieldVals.drop(clazz.fields.size), classes)
    }
  }

  /**
   * Returns the first method found in the path from className to the root in
   * the inheritance tree or None
   */
  def lookupMethod(
    methodName: Symbol,
    className: Symbol,
    classes: Map[Symbol, Class]): Option[Method] = className match {
    // We reached the top and found no method ('Object is root of all classes)
    case 'Object => None
    case _ => {
      // Retrieve Class definition for className
      val clazz = classes.getOrElse(
        className,
        sys.error("Unknown class %s".format(className)))
      // Search for method. If not present, search in super class
      if (clazz.methods.contains(methodName)) Some(clazz.methods(methodName))
      else lookupMethod(methodName, clazz.superClass, classes)
    }
  }

  def interp(e: Expr, env: Env, classes: Map[Symbol, Class]): Value = e match {
    case New(className, args) => {
      if (!classes.contains(className))
        sys.error("Can not initialize unknown class %s".format(className))
      Object(className, args map { interp(_, env, classes) })
    }

    case FAcc(objExpr, fieldName) => {
      val maybeObj = interp(objExpr, env, classes)
      maybeObj match {
        case Object(className, fields) =>
          lookupField(fieldName, className, fields, classes)
        case _ => sys.error("Expected object, but got %s".format(maybeObj))
      }
    }

    case Invoke(objExpr, methodName, args) => {
      val maybeObj = interp(objExpr, env, classes)
      maybeObj match {
        case Object(className, fieldVals) => {
          val method = lookupMethod(methodName, className, classes) getOrElse
            sys.error("Unknown method %s for class %s".format(methodName, className))
          val argVals = args map { interp(_, env, classes) }
          val argBindings = method.params zip argVals
          val thisBinding = 'this -> maybeObj
          val newEnv = Map() ++ argBindings + thisBinding
          interp(method.body, newEnv, classes)
        }
        case _ => sys.error("Expected object, but got %s".format(maybeObj))
      }
    }

    case Id(id) => env(id)
  }

  val testclasses = Map(
    'True -> Class('Object, List.empty, Map(
      'ifThenElse -> Method(List('thenExp, 'elseExp), Id('thenExp)),
      'and -> Method(List('x), Id('x)))),
    
    'False -> Class('Object, List.empty, Map(
      'ifThenElse -> Method(List('thenExp, 'elseExp), Id('elseExp)),
      'and -> Method(List('x), Id('this)))),
     
    'Food -> Class('Object, List('organic), Map(
      'tastesBetterThan ->
        Method(
          // Parameter for tastesBetterThan
          List('other),
          // Body of method
          // Invoke(objExpr, methodName, args: List)
          // Rufe auf objExpr methodName mit args auf
                 // Rufe auf this das Feld organic ab, organic ist True/False class Binding entsteht später
          Invoke(FAcc(Id('this), 'organic),
            'ifThenElse,
            List(
              // Parameter for ifThenElse method see above
              New('True, List.empty), // thenExpr
              FAcc(Id('otherFood), 'organic)))))), // elseExpr
    'Pizza -> Class('Food, List('hasCheese), Map(
      'tastesBetterThan ->
        Method(
          List('other),
          Invoke(
             // Call on organic (True, False class) and method with parameter hashCheese on this object
            FAcc(Id('this), 'organic),
            'and, List(FAcc(Id('this), 'hasCheese)))))))

  val testRes = 
    interp(
      Invoke(
        New('Pizza, List(New('True, List.empty), New('True, List.empty))),
        'tastesBetterThan,
        List(New('Food, List(New('True, List.empty))))),
      Map(), testclasses)
      
  println(testRes)
      
  assert(
      testRes
      ==
      Object('True, List.empty))
}
