val captailOfContry = Map("US"->"washenton", "China"->"Beijing")

val strToInt = Map("Lee" ->2, "Nannan"->24)
val strToInt1 = Map("Lee"->2, "Nannan"->3)

strToInt ++ strToInt1

captailOfContry("China")

captailOfContry get "China"

captailOfContry get "ABC"

def getCaptal(country:String) = captailOfContry get country match {
  case Some(x) => x
  case None => "missing data"
}

getCaptal("China")
getCaptal("ABC")

//captailOfContry("England")


val fruit = List("Apple", "Orange", "Banana", "Pineapple", "Pear")

fruit sortWith( _.length < _.length)

fruit sorted

fruit groupBy(_.head)

