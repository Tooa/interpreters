/*

  > (1) We have a function with its fixpoint x:
    x = f(x)

  > (2) We define a fix function that returns the
    fixpoint of a function:
    x = fix(f)

    Enroll (2) in (1) -> fix(f) = f(fix(f)) (fix definition)
 */

def fix[A,B](f: (A=>B)=>(A=>B)): A=>B = (x:A) => f(fix(f))(x)

def factorial(recursive: Long => Long) = (x: Long) => x match {
  case 1 => 1
  case x => x * recursive(x-1)
}

def length(recursive: List[Int] => Int) = (l: List[Int]) => l match {
  case Nil => 0
  case x::xs => 1 + recursive(xs)
}

fix(factorial)(3)
/*
=> factorial(fix(factorial))(3)) 		    // Call factorial(...)(3)
=> 3 * fix(factorial)(2)				        // Call fix(...)(2)
=> 3 * factorial(fix(factorial))(2)     // Call factorial(...)(2)
=> 3 * 2 * fix(factorial)(1) 			      // Call fix(...)(1)
=> 3 * 2 * factorial(fix(factorial))(1) // Call factorial(...)(1)
=> 3 * 2 * 1
=> 6
 */

fix(length)(List(1,2,3,4,5))