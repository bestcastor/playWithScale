//****************
//  Array
val array = Array(1, 2, 6, 44, 8)

array map (x => x*2) //2, 4, 12, 88, 16




//***************
// String

val string = "Hello World!"

string filter (c => c.isUpper) //HW


//************* Common operators of Seq

string exists( c => c.isUpper) // true
string forall ( c => c.isUpper) // false

val pairs = List(1, 2, 3, 4, 5, 6, 7, 8) zip string
pairs.unzip

string flatMap( c => List('.', c, '# ))

array.sum
array.max
array.min
array.product


//***************** Examples
//get the combinations of 1 to N and 1 to M
7 to 9 flatMap (x => 1 to 5 map(y =>  (x, y)))


//Get the produce of two vectors

def scalarProduce1(v1:Vector[Double], v2:Vector[Double]):Double =
  (v1 zip v2).map(v12 => v12._1 * v12._2).sum

def scalarProduce2(v1:Vector[Double], v2:Vector[Double]):Double =
  (v1 zip v2).map {case(x, y) => x * y }.sum

//We don't think about the efficience, just give a abstract function.
def isPrime(n: Int) = 2 to n-1 forall(d => n %d != 0)

isPrime(37)