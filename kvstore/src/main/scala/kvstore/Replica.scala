package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class CheckAck(key: String, id: Long)
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  var kv = Map.empty[String, String]
  var expectedSeq = 0L

  var secondaries = Map.empty[ActorRef, ActorRef] //replica -> replicator

  var replicators = Set.empty[ActorRef]

  var persistent: ActorRef = context.actorOf(persistenceProps)

  type Identifier = (String, Long)

  case class AckWorkflow(
    key: String,
    id: Long,
    pendingReplicators: Set[ActorRef] = Set(),
    requester: ActorRef,
    valueOption: Option[String],
    persistenceDone: Boolean = false) {
    val finished: Boolean = persistenceDone && pendingReplicators.isEmpty
    val identifier: Identifier = (key, id)
  }

  object Works {
    private var pending = Map.empty[Identifier, AckWorkflow] //(key,id) -> ack

    def persistenceDone(id: Identifier): AckWorkflow = {
      val workflow = get(id)
      val newWorkflow = workflow.copy(persistenceDone = true)
      pending += workflow.identifier -> newWorkflow
      newWorkflow
    }

    def replicationDone(id: Identifier, replicator: ActorRef): AckWorkflow = {
      val workflow = get(id)
      val newWorkflow = workflow.copy(pendingReplicators = workflow.pendingReplicators - replicator)
      pending += workflow.identifier -> newWorkflow
      newWorkflow

    }

    def remove(workflow: AckWorkflow) = {
      pending -= workflow.identifier
    }

    def registerPersistence(id: Identifier) = {
      val workflow = get(id)
      pending += workflow.identifier -> workflow.copy(persistenceDone = false)

    }

    def registerReplication(id: Identifier, replicator: ActorRef) {
      val workflow = get(id)
      pending += workflow.identifier -> workflow.copy(pendingReplicators = workflow.pendingReplicators + replicator)

    }

    def get(id: Identifier): AckWorkflow = {
      pending(id)
    }

    def exist(id: Identifier): Boolean = {
      pending.get(id) match {
        case Some(_) => true
        case None => false
      }
    }

    def add(key: String, id: Long, valueOption: Option[String], requester: ActorRef): Identifier = {
      val workflow = pending.getOrElse((key, id), AckWorkflow(key = key, id = id, requester = requester, valueOption = valueOption))
      pending += workflow.identifier -> workflow
      workflow.identifier
    }

    def removeReplicator(toRemove: ActorRef) {
      pending.keys.foreach { id =>
        val workflow = replicationDone(id, toRemove)
        if (workflow.finished) {
          workflow.requester ! OperationAck(workflow.id)
          remove(workflow)
        }
      }
    }

    def pendings = Works.pending.values.filterNot(_.persistenceDone)
  }

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      replicate(key, Some(value), id)
    }

    case Remove(key, id) => {
      kv -= key
      replicate(key, None, id)
    }

    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id) => {
      if (Works.exist((key, id))) {
        val workflow = Works.persistenceDone((key, id))
        if (workflow.finished) {
          workflow.requester ! OperationAck(workflow.id)
          Works.remove(workflow)
        }
      }
    }

    case Replicated(key, id) => {
      if (Works.exist((key, id))) {
        val workflow = Works.replicationDone((key, id), sender)
        if (workflow.finished) {
          workflow.requester ! OperationAck(workflow.id)
          Works.remove(workflow)
        }
      }
    }

    case CheckAck(key, id) => {
      if (Works.exist((key, id))) {
        val workflow = Works.get((key, id))
        workflow.requester ! OperationFailed(id)
        Works.remove(workflow)
      }
    }

    case Replicas(replicas) => {
      val added = replicas -- secondaries.keySet - self
      val removed = secondaries.keySet -- replicas
      removed.foreach { r =>
        Works.removeReplicator(secondaries(r))
        context stop secondaries(r)
        secondaries -= r
      }

      added.foreach { a =>
        val replicator = context.actorOf(Replicator.props(a))
        secondaries += a -> replicator
        kv.foreach {
          case (k, v) => replicator ! Replicate(k, Some(v), secondaries.size)
        }
      }

    }
  }

  val replica: Receive = {

    case Snapshot(key, valueOption, seq) => {
      if (seq == expectedSeq) {
        expectedSeq += 1
        update(key, valueOption)
        persist(key, valueOption, seq)
      } else if (seq < expectedSeq)
        sender ! SnapshotAck(key, seq)
    }

    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id) => {
      if (Works.exist((key, id))) {
        val workflow = Works.persistenceDone((key, id))
        if (workflow.finished) {
          workflow.requester ! SnapshotAck(key, workflow.id)
          Works.remove(workflow)
        }
      }
    }
  }

  def replicate(key: String, valueOption: Option[String], id: Long) {
    val w = Works.add(key, id, valueOption, sender)

    secondaries.values.foreach { replicator =>
      Works.registerReplication(w, replicator)
      replicator ! Replicate(key, valueOption, id)
    }

    persist(key, valueOption, id)
    context.system.scheduler.scheduleOnce(1 second, self, CheckAck(key, id))
  }

  def persist(key: String, valueOption: Option[String], id: Long) {
    val w = Works.add(key, id, valueOption, sender)
    Works.registerPersistence(w)

    val p = Persist(key, valueOption, id)

    persistent ! p
  }

  def update(key: String, valueOption: Option[String]) {
    if (valueOption.isDefined)
      kv += key -> valueOption.get
    else
      kv -= key
  }

  override def preStart(): Unit = {

    def tryPersist(): Unit = {
      Works.pendings.foreach { w =>
        persistent ! Persist(w.key, w.valueOption, w.id)
      }
    }

    arbiter ! Join
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds)(tryPersist)

  }

}