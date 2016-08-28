//We don't think about the efficience, just give a abstract function.
def isPrime(n: Int) = 2 to n-1 forall(d => n %d != 0)

val  n = 7

(1 until n map(i => 1 until i map(j => (i, j)))).flatten

//equal to
(1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (
  pair => isPrime(pair._1 + pair._2)
  )

//equal to
for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)


def scalarProduce(v1:Vector[Double], v2:Vector[Double]) =
  (for ((x,y) <- v1 zip v2) yield x*y).sum

val vec = Vector(List(1, 1), List(2, 1), List(4, 7), List(7, 8))

vec.flatten


case class Person(name: String, age:Int)

val persons = List[Person]()

for (person <- persons if person.age > 20) yield person.name

//equal to

persons filter (person => person.age > 20) map (person => person.name)

