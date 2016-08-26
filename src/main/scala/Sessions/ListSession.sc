import math.Ordering

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x <= y) x :: xs
    else y :: insert(x, ys)
  }
}

//*************
def isort(xs : List[Int]) : List[Int] = xs match {
  case List()  => List()
  case y :: ys => insert(y, isort(ys))
}


def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

///TEST
val elem = List(7,3,2, 6)
val sortE = isort(elem)

//****************
def removeAt[T](n: Int, xs: List[T]) : List[T]= xs match {
  case List() => throw new Error("remove at n")
  case y :: ys => {
    if (n == 0) ys
    else y :: removeAt(n-1, ys)
  }
}
/// Test code

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

//********************

def msort[T](xs:List[T])(implicit ord: Ordering[T]) : List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(ys:List[T], zs:List[T]):List[T] = (ys, zs) match {
      case (Nil, zs) => zs
      case (ys, Nil) => ys
      case (y1::ys1, z1::zs1) => {
        if (ord.lt(y1, z1)) y1::merge(ys1, zs)
        else z1 :: merge(ys, zs1)
      }
    }

    val (fst, lst) = xs splitAt n

    merge (msort(fst), msort(lst))
  }

}

///Test
val l1 = List(8, -4, 2, 5, 9, 7, -1)
msort(l1)

val fruits = List("apple", "pineapple", "orange", "banana")
msort(fruits)

l1 filter (x => x > 0)
l1 filterNot(x => x>0)
l1 partition (x => x>0)

l1 takeWhile( x => x !=5)
l1 dropWhile (x => x !=5)
l1 span ( x=> x != 5)
//l1 takeRight( x => x==5)

//********

def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y*y :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)


//************
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => List((xs.takeWhile(y => y ==x))) ::: pack(xs1.dropWhile(y => y==x))
}


pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode(xs : List[String]) : List[(String, Int)] = {
  val lists = pack(xs)
  //println(lists)
  var result = List[(String, Int)]()

  lists.foreach( list => {
    val pair = (list.head, list.length)
    result = result ::: List(pair)
    //println(pair)
  })
  //println(result)
  result
}
//Better solution
def encode2[T](xs : List[T]) : List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))
encode2(List("a", "a", "a", "b", "c", "c", "a"))


val chars = scala.collection.mutable.MutableList[Char]('a', 'b', 'c')

chars.toList

chars += 'd'


