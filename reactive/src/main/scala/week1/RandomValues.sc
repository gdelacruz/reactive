package week1

object RandomValues {
  trait GeneratorDummy[+T] {
    def generate: T
  }

  trait Generator[+T] { self => // an alias for ”this”.
    def generate: T
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> integers  : week1.RandomValues.Generator[Int]{val rand: java.util.Random} = 
                                                  //| week1.RandomValues$$anonfun$main$1$$anon$3@1f7c9157

  val booleans = for (x <- integers) yield x > 0  //> booleans  : week1.RandomValues.Generator[Boolean] = week1.RandomValues$Gener
                                                  //| ator$$anon$1@31506701

  val booleans2 = integers.map(x => x > 0)        //> booleans2  : week1.RandomValues.Generator[Boolean] = week1.RandomValues$Gene
                                                  //| rator$$anon$1@5559c7f2

  booleans.generate                               //> res0: Boolean = true

  booleans2.generate                              //> res1: Boolean = false

  def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
    x => u map { y => (x, y) }
  }                                               //> pairs: [T, U](t: week1.RandomValues.Generator[T], u: week1.RandomValues.Gene
                                                  //| rator[U])week1.RandomValues.Generator[(T, U)]

  def pairs2[T, U](t: Generator[T], u: Generator[U]) = t.flatMap(x => u.map(y => (x, y)))
                                                  //> pairs2: [T, U](t: week1.RandomValues.Generator[T], u: week1.RandomValues.Gen
                                                  //| erator[U])week1.RandomValues.Generator[(T, U)]

  pairs(integers, integers).generate              //> res2: (Int, Int) = (-1254920744,-116982320)

  pairs2(integers, integers).generate             //> res3: (Int, Int) = (-1234425270,532445346)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }                                               //> single: [T](x: T)week1.RandomValues.Generator[T]
  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)  //> choose: (lo: Int, hi: Int)week1.RandomValues.Generator[Int]

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)
                                                  //> oneOf: [T](xs: T*)week1.RandomValues.Generator[T]

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list                                    //> lists: => week1.RandomValues.Generator[List[Int]]

  def emptyLists = single(Nil)                    //> emptyLists: => week1.RandomValues.Generator[scala.collection.immutable.Nil.
                                                  //| type]

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail                            //> nonEmptyLists: => week1.RandomValues.Generator[List[Int]]

  //TREES

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree                                    //> trees: => week1.RandomValues.Generator[week1.RandomValues.Tree]

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)                                 //> leafs: => week1.RandomValues.Generator[week1.RandomValues.Leaf]

  def inners: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)                      //> inners: => week1.RandomValues.Generator[week1.RandomValues.Inner]

  trees.generate                                  //> res4: week1.RandomValues.Tree = Leaf(1091815853)

  //TEST

  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + " tests")
  }                                               //> test: [T](g: week1.RandomValues.Generator[T], numTimes: Int)(test: T => Boo
                                                  //| lean)Unit

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }                                               //> java.lang.AssertionError: assertion failed: test failed for (List(432273392
                                                  //| , 2002339910, -2064747768, -189268481, -2088000105),List())
                                                  //| 	at scala.Predef$.assert(Predef.scala:179)
                                                  //| 	at week1.RandomValues$$anonfun$main$1$$anonfun$test$1$1.apply$mcVI$sp(we
                                                  //| ek1.RandomValues.scala:90)
                                                  //| 	at scala.collection.immutable.Range.foreach$mVc$sp(Range.scala:141)
                                                  //| 	at week1.RandomValues$$anonfun$main$1.test$1(week1.RandomValues.scala:88
                                                  //| )
                                                  //| 	at week1.RandomValues$$anonfun$main$1.apply$mcV$sp(week1.RandomValues.sc
                                                  //| ala:95)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week1.RandomValues$.main(week1.RandomValues.scala:17)
                                                  //| 	at week1.RandomValues.mai
                                                  //| Output exceeds cutoff limit.

}