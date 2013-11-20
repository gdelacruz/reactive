package week2.simulation

trait Simulation {

    type Action = () => Unit
    case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()

    private var curtime = 0

    /** The current simulated time */
    def currentTime: Int = curtime
    /**
     * Registers an action ‘block‘ to perform after a given delay
     * relative to the current time
     */
    def afterDelay(delay: Int)(block: => Unit): Unit = {
      val item = Event(currentTime + delay, () => block)
      agenda = insert(agenda, item)
    }

    private def insert(ag: List[Event], item: Event): List[Event] = ag match {
      case first :: rest if first.time <= item.time =>
        first :: insert(rest, item)
      case _ =>
        item :: ag
    }

    private def loop(): Unit = agenda match {
      case first :: rest =>
        agenda = rest
        curtime = first.time
        first.action()
        loop()
      case Nil =>
    }

    /** Performs the simulation until there are no actions waiting */
    def run(): Unit = {
      afterDelay(0) {
        println("*** simulation started, time = ”+currentTime+” ***")
      }
      loop()
    }
  }