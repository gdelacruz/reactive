package week2

object Test {
  object sim extends Circuits with Parameters
  
  import sim._
  

  val in1, in2, sum, carry = new Wire             //> in1  : week2.Test.sim.Wire = week2.Gates$Wire@3e61bea7
                                                  //| in2  : week2.Test.sim.Wire = week2.Gates$Wire@5b065904
                                                  //| sum  : week2.Test.sim.Wire = week2.Gates$Wire@7dbe444b
                                                  //| carry  : week2.Test.sim.Wire = week2.Gates$Wire@5675d86b
  
  halfAdder(in1, in2, sum, carry)
  
  probe("sum", sum)                               //> sum 0 value = false
  probe("carry", carry)                           //> carry 0 value = false

	in1 setSignal true
	
	run()                                     //> *** simulation started, time = ”+currentTime+” ***
                                                  //| sum 8 value = true

	in2 setSignal true
	
	run()                                     //> *** simulation started, time = ”+currentTime+” ***
                                                  //| carry 11 value = true
                                                  //| sum 16 value = false
}