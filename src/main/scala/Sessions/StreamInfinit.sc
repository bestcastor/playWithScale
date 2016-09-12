//For instance, here is the stream of all integers starting from a given number:
def from(n: Int): Stream[Int] = n #:: from(n+1)
//The stream of all natural numbers:
val nats = from(0)
//The stream of all multiples of 4:
  nats map (_ * 4) take 100 toList

//This expression will fail!
//nats.toList


//Here’s a function that implements this principle:
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))
val primes = sieve(from(2))
//To see the list of the first N prime numbers, you can write
(primes take 100).toList


def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(100).take(10).toList


def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0000000000000001
sqrtStream(4) filter (isGoodEnough(_, 4))