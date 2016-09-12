import commonUtil.UtilO._

//For instance, to find the second prime number between 1000 and 10000:
// [Way 1]
// - The performance is not good because we need to run isPrime
// - for all the members.
((1000 to 10000) filter isPrime)(1)

// [Way 2]
//  -- This is much longer than [way 1] alternative;
// -- But performance will be much better
def secondPrime(from: Int, to: Int) = nthPrime(from, to, 2)
def nthPrime(from: Int, to: Int, n: Int): Int =
  if (from >= to) throw new Error("no prime")
else if (isPrime(from))
  if (n == 1) from else nthPrime(from + 1, to, n - 1)
else nthPrime(from + 1, to, n)

secondPrime(1000, 10000)

// [Way 3]
// We try to use the 'stream' to save the performance
(1000 to 10000).toStream

((1000 to 10000).toStream filter isPrime)(1)

// Note the big difference is here:
// -- Also, stream is used by "callbyName"
// -- List is used by 'call by vallue'.
val list = List(3, 4 ,5)
val stream = Stream(3, 4, 5)
1 :: list
1 #:: stream


def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo+" " )
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).take(3).toList //only print 1, 2, 3


def listRange(lo: Int, hi: Int): List[Int] = {
  print(lo+" " )
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)
}

listRange(1, 10).take(3) //print 1..10