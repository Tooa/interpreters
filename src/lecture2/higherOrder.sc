

def summing(list: List[Int]): Int = list.foldRight(0)(_ + _)
def product(list: List[Int]): Int = list.foldLeft(1)(_ * _)
//b is accumulator result of chained f(f( ... (x)) before
//For foldLeft f(TypeOfInit/Accumulator, TypeOfList)
def length[A](list: List[A]): Int = list.foldLeft(0)((a: Int, b: A) => a + 1)
def reverse[A](list: List[A]): List[A] = list.foldLeft(List[A]())((b: List[A], a: A) => a :: b)
def myMap[A, B] (f: A => B, list: List[A]): List[B] = list.foldRight(List[B]())((b: A, a: List[B]) => f(b) :: a)
def myFilter[A](p: A => Boolean, list: List[A]): List[A] = list.foldRight(List[A]())((b: A, a: List[A]) => if(p(b)) b :: a else a)

summing(List(1, 4, 5))
product(List(1, 4, 5))
length(List(1, 4, 5, 3, 1))
reverse(List(1, 4, 5))
myMap((x:Int) => 2*x, List(1, 4, 5))
myFilter((x: Int) => x > 2, List(1, 4, 5))

def factorialRec(n: Int): Int = if(n == 0) 1 else n*factorialRec(n-1)
def factorialFold(n: Int): Int = List.range(1, n).foldRight(n)(_ * _)

factorialRec(10)
factorialFold(10)


