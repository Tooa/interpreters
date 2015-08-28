
/*
  “Traditional code” can be transformed automatically into
   a form where every call is a tail call! Such a transformation
   is said to bring a function into Continuation Passing Style (CPS).
 */

// k: Int => Int <-- result type of k ): Int <-- result type of k
def factCPS(n: Int, k: Int => Int): Int = n match {
  case 1 => k(1)
  case n => factCPS(n-1, { res => k(n* res) })
}

factCPS(3, identity)

def sumCPS(n: Int, k: Int => Int): Int = n match {
  case 1 => k(1)
  // Important to call the outer continuation k
  case n => sumCPS(n-1, { result => k(result + n) })
}

sumCPS(4, identity)

def lengthCPS(l: List[Int], k: Int => Int): Int = l match {
  // Don't miss to call k here
  case Nil => k(0)
  case x::xs => lengthCPS(xs, { result => k(1 + result) })
}

lengthCPS(List(1,2,3,4), identity)


def sumListCPS(l: List[Int], k: Int => Int): Int = l match {
  case Nil => k(0)
  case x::xs => sumListCPS(xs, { result => k(x + result) })
}

sumListCPS(List(1,2,3,4), identity)


