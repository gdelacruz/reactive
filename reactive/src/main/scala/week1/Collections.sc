package week1

object Collections {
  val N = 10                                      //> N  : Int = 10

  val a = for {
    x <- 2 to N
    y <- 2 to x
    if (x % y == 0)
  } yield (x, y)                                  //> a  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,2), (3,3)
                                                  //| , (4,2), (4,4), (5,5), (6,2), (6,3), (6,6), (7,7), (8,2), (8,4), (8,8), (9,3
                                                  //| ), (9,9), (10,2), (10,5), (10,10))

  val b = (2 to N) flatMap (x =>
    (2 to x) withFilter (y =>
      x % y == 0) map (y => (x, y)))              //> b  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,2), (3,3)
                                                  //| , (4,2), (4,4), (5,5), (6,2), (6,3), (6,6), (7,7), (8,2), (8,4), (8,8), (9,3
                                                  //| ), (9,9), (10,2), (10,5), (10,10))
  a == b                                          //> res0: Boolean = true

}