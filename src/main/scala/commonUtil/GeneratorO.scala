package commonUtil

/**
  * Created by nyu on 9/11/16.
  */
object GeneratorO {
  //Let’s define a trait Generator[T] that generates random values of type T:
  trait Generator[+T] {
    self => // an alias for ”this”.
    def generate: T
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  //Some instances:
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  val pairs = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }



  //A more generic way:
  val booleansNew = for (x <- integers) yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)



  //A common Generator:
  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + Math.abs(x) % (hi - lo) //There is a bug in the original code. We need to filter the negtive value.
  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  //A List generator
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
  def emptyLists = single(Nil)
  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  //Define a tree generator
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def leafs : Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners : Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
  } yield if(isLeaf) leafs.generate else inners.generate

}
