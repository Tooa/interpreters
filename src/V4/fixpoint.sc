def fix[A,B](f: (A=>B)=>(A=>B)): A=>B = (x:A) => f(fix(f))(x)

def factorial(recursive: Long => Long) = (x: Long) => x match {
  case 1 => 1
  case x => x * recursive(x-1)
}

fix(factorial)(3)