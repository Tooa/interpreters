

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


abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

def sumTree(t: Tree[Int]): Int = t match {
  case Leaf(a) => a
  case Node(l, r) => sumTree(l) + sumTree(r)
}

def collectTree[A](t: Tree[A]): Set[A] = t match {
  case Leaf(a) => Set(a)
  case Node(l, r) => collectTree(l) ++ collectTree(r)
}

def mapTree[A, B](t: Tree[A], op: A => B): Tree[B] = t match {
  case Leaf(a) => Leaf(op(a))
  case Node(l,r) => Node(mapTree(l, op), mapTree(r, op))
}

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(a) => 0
  case Node(l, r) => 1 + Math.max(depth(l), depth(r))
}

def foldTree[A, B](t: Tree[A])(nodeOp: A => B)(op: (B, B) => B): B = t match {
  case Leaf(a) => nodeOp(a)
  case Node(l, r) => op(foldTree(l)(nodeOp)(op), foldTree(r)(nodeOp)(op))
}

val t = Node(Node(Leaf(2), Leaf(1)),Node(Node(Leaf(1), Leaf(3)), Node(Leaf(1), Leaf(5))))


foldTree(t)(identity)(_ + _)
foldTree(t)(Set(_))(_ ++ _)
foldTree(t)((a) => 0)(1 + Math.max(_, _))

// We are forced to add type annotation to result of op otherwise we get a type error
foldTree(t)((x: Int) => Leaf(x + 2): Tree[Int])(Node(_, _))


