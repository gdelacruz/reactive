package week1

import scala.util.control.NonFatal

object Monads {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  abstract class Try[+T] {
    def flatMap[U](f: T => Try[U]): Try[U] = this match {
      case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
      case fail: Failure => fail
    }
    def map[U](f: T => U): Try[U] = this match {
      case Success(x) => Try(f(x))
      case fail: Failure => fail
    }
  }

  object Try {
    def apply[T](expr: => T): Try[T] =
      try Success(expr)
      catch {
        case NonFatal(ex) => Failure(ex)
      }
  }

  case class Success[T](x: T) extends Try[T]
  case class Failure(ex: Throwable) extends Try[Nothing]

  //Try no cumple la left unit law. Entonces NO ES UN MONAD. Pero como la left unit law no tiene una aplicacion practica para el uso en for,
  //Sigue siendo valido usarla

  /* El compute seria el try, como f(x,y) use una que me devuelva el resultado como un Pair
		for {
			x <- computeX
			y <- computeY
		} yield f(x, y)
	*/

  def compute[T, U](expr1: => T, expr2: => U) = for {
    x <- Try(expr1)
    y <- Try(expr2)
  } yield (x, y)                                  //> compute: [T, U](expr1: => T, expr2: => U)week1.Monads.Try[(T, U)]

  compute(2 * 3, 4 * 6)                           //> res0: week1.Monads.Try[(Int, Int)] = Success((6,24))

  // En el caso de una falla se va porque tanto el map como el flatmap si falla ya tiran el fail (no ejecutan la funcion)
  // Entonces pasa por arriba todo el comportamiento
  // Esto es por lo que no cumple la left unit law ya que era el comportamiento deseado (Si falla falla, sino dame los resultados
  // Del success

  compute(2 / 0, 4 * 6)                           //> res1: week1.Monads.Try[(Int, Int)] = Failure(java.lang.ArithmeticException:
                                                  //|  / by zero)
  compute(2 * 3, 4 / 0)                           //> res2: week1.Monads.Try[(Int, Int)] = Failure(java.lang.ArithmeticException:
                                                  //|  / by zero)

  // Aca la converti a flatmap y map para entender el concepto de la falla
  def compute2[T, U](expr1: => T, expr2: => U) =
    Try(expr1).flatMap(x => Try(expr2).map(y => (x, y)))
                                                  //> compute2: [T, U](expr1: => T, expr2: => U)week1.Monads.Try[(T, U)]

  compute2(2 * 3, 4 * 6)                          //> res3: week1.Monads.Try[(Int, Int)] = Success((6,24))

  compute2(2 / 0, 4 * 6)                          //> res4: week1.Monads.Try[(Int, Int)] = Failure(java.lang.ArithmeticException:
                                                  //|  / by zero)
}