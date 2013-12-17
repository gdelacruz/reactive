/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case o: Operation => root ! o
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished =>
      root ! PoisonPill
      pendingQueue.foreach { newRoot ! _ }
      pendingQueue = Queue.empty[Operation]
      root = newRoot
      context.unbecome()

    case GC => ()

    case msg: Operation =>
      pendingQueue = pendingQueue.enqueue(msg)

  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case i @ Insert(requester, id, e) =>
      if (e == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else {
        val position = if (e < elem) Right else Left
        subtrees.get(position) match {
          case None =>
            subtrees += (position -> context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false)))
            requester ! OperationFinished(id)
          case Some(leaf) => leaf ! i
        }
      }
    case r @ Remove(requester, id, e) =>
      if (e == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val position = if (e < elem) Right else Left
        subtrees.get(position) match {
          case None =>
            requester ! OperationFinished(id)
          case Some(leaf) => leaf ! r
        }

      }

    case c @ Contains(requester, id, e) =>
      if (e == elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        val position = if (e < elem) Right else Left
        subtrees.get(position) match {
          case None =>
            requester ! ContainsResult(id, false)
          case Some(leaf) => leaf ! c
        }
      }

    case c @ CopyTo(newRoot) =>
      val children = subtrees.values

      if (!removed) {
        newRoot ! Insert(self, -1, elem)
        context.become(copying(subtrees.values.toSet, false))
      } else if (subtrees.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(subtrees.values.toSet, true))
      }

      children.foreach { _ ! c }

  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => {
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
    }
    case OperationFinished(-1) => {
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected, true))
      }
    }
  }

}
