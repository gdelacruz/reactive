package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // Time to incubate the virus
    val incubationTime = 6
    // Time to be sick (if not dead...)
    val sickTime = 8
    // After being sick, Time to be immuned, but sick...
    val inmuneTime = 2
    // Time to loose immunity and being healthy again...
    val healTime = 2

    val dieRate = 0.25
    val infectedPopulationRate = 0.01
    val transmissionRate = 0.4
  }

  import SimConfig._

  val persons: List[Person] = (0 until population).toList.map(new Person(_))

  //Initial infection
  persons.take((population * infectedPopulationRate).intValue).foreach(_.infect)

  //Schedule all persons initial moves
  persons.foreach(_.scheduleMove)

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def alive = !dead

    def healthy = !infected

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isNeighboor(p: Person) = row == p.row && col == p.col

    def isMyPlace(room: (Int, Int)) = room match {
      case (r, c) => row == r && col == c
    }

    def neighbors =
      List((row - 1 + roomRows) % roomRows, (row + 1) % roomRows).map((_, col)) ++
        List((col - 1 + roomColumns) % roomColumns, (col + 1) % roomColumns).map((row, _))

    def infect {
      infected = true
      afterDelay(incubationTime)(getVirus)
      afterDelay(incubationTime + sickTime)(kill)
      afterDelay(incubationTime + sickTime + inmuneTime) { if (alive) immunize }
      afterDelay(incubationTime + sickTime + inmuneTime + healTime) { if (alive) backHealthy }
    }

    //Lifecycle
    def getVirus {
      assert(infected)
      sick = true
    }

    def kill = {
      assert(sick)

      if (random < dieRate) {
        dead = true
      }
    }

    def immunize = {
      assert(alive)
      assert(sick)

      sick = false
      immune = true
    }

    def backHealthy = {
      assert(alive)
      assert(immune)

      immune = false
      infected = false

    }

    def scheduleMove() {
      val moveDelay = randomBelow(5) + 1
      afterDelay(moveDelay)(if (alive) move)
    }

    def move(nextRoom: (Int, Int)) = nextRoom match {
      case (r, c) => row = r; col = c
    }

    def move() {

      assert(alive)

      val safeNeighbors = neighbors.filter(room => !persons.exists(p => p.isMyPlace(room) && (p.sick || p.dead)))
      if (!safeNeighbors.isEmpty) {
        move(safeNeighbors(randomBelow(safeNeighbors.length)))
      }
      // Si estoy sano
      if (healthy)
        if (random < transmissionRate) {
          if (persons.exists(person => isNeighboor(person) && person.infected))
            infect
        }
      scheduleMove()
    }

  }
}
