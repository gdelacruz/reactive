package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "and 3")
    
    in1.setSignal(true)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "and 4")

  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "and 3")
    
    in1.setSignal(true)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "and 4")
  }
  

  test("demux example") {
    val in1= new Wire
    val control = List(new Wire, new Wire)
    val out = List(new Wire, new Wire, new Wire, new Wire)
    
    demux(in1, control, out)

    in1.setSignal(true)
    
    control.foreach(_.setSignal(false))
    
    run
    
    assert(out.filterNot(out => out.getSignal == false).size == 1)
    assert(out(3).getSignal == true)

    control(0).setSignal(false)
    control(1).setSignal(true)
    run
    
    assert(out.filterNot(out => out.getSignal == false).size == 1)
    assert(out(2).getSignal == true)


    control(0).setSignal(true)
    control(1).setSignal(false)
    run
    
    assert(out.filterNot(out => out.getSignal == false).size == 1)
    assert(out(1).getSignal == true)

    control(0).setSignal(true)
    control(1).setSignal(true)
    run
    
    assert(out.filterNot(out => out.getSignal == false).size == 1)
    assert(out(0).getSignal == true)
  }
}
