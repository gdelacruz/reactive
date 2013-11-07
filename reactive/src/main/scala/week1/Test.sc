package week1

object Test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val f: String => String = { case "ping" => "pong" }
                                                  //> f  : String => String = <function1>

  f("ping")                                       //> res0: String = pong

  //f("abc")
  val pf: PartialFunction[String, String] = { case "ping" => "pong" }
                                                  //> pf  : PartialFunction[String,String] = <function1>

  pf.isDefinedAt("ping")                          //> res1: Boolean = true

  pf.isDefinedAt("pong")                          //> res2: Boolean = false

  val pf2: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }                                               //> pf2  : PartialFunction[List[Int],String] = <function1>

  pf2.isDefinedAt(1 :: 2 :: Nil)                  //> res3: Boolean = true

  val pf3: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest => rest match {
      case Nil => "two"
    }
  }                                               //> pf3  : PartialFunction[List[Int],String] = <function1>

  pf3.isDefinedAt(1 :: 2 :: Nil)                  //> res4: Boolean = true

  // pf3(1::2::Nil)
}