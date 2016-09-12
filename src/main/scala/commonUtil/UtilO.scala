package commonUtil

/**
  * Created by nyu on 9/11/16.
  */
object UtilO {

  //We don't think about the efficience, just give a abstract function.
  def isPrime(n: Int) = 2 to n-1 forall(d => n %d != 0)

}
