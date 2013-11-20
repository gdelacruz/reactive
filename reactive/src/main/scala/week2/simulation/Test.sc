package week2.simulation

object Test {
  object sim extends Circuits with Parameters
  
  import sim._
  

  val in1, in2, sum, carry = new Wire             //> in1  : week2.simulation.Test.sim.Wire = week2.simulation.Gates$Wire@63d1e70a
                                                  //| 
                                                  //| in2  : week2.simulation.Test.sim.Wire = week2.simulation.Gates$Wire@1732781b
                                                  //| 
                                                  //| sum  : week2.simulation.Test.sim.Wire = week2.simulation.Gates$Wire@726b4082
                                                  //| 
                                                  //| carry  : week2.simulation.Test.sim.Wire = week2.simulation.Gates$Wire@5f5851
                                                  //| 7d
  
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