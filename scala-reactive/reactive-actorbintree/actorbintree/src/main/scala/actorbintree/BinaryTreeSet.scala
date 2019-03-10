/**
  * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
  */
package actorbintree

import java.util
import java.util.Comparator
import java.util.concurrent.TimeUnit

import akka.actor._
import scala.collection.immutable.Queue
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import akka.pattern.ask
import akka.util.Timeout

object BinaryTreeSet {


  implicit val timeout = Timeout(10, TimeUnit.SECONDS)

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {

  import BinaryTreeSet._
  import BinaryTreeNode._
  import context.dispatcher

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = List.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  def normal: Receive = {
    case x: Insert   => root ! x
    case x: Contains => root ! x
    case x: Remove   => root ! x
    case GC          =>
      val newRoot = createRoot
      root ask CopyTo(newRoot) foreach (self ! _)
      context.become(garbageCollecting)
      root = newRoot
  }


  def garbageCollecting: Receive = {
    case x: Operation =>
      pendingQueue = x :: pendingQueue
    case CopyFinished =>
      context.become(normal)
      pendingQueue.reverse.foreach(root ! _)
      pendingQueue = Nil
  }

}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._
  import context.dispatcher

  var subtrees = Map[Position, ActorRef]()
  var isRemove = initiallyRemoved

  def receive = normal


  def copyReceive: Receive = {
    case x@CopyTo(treeNode) =>
      val send = sender()
      if(!isRemove) treeNode ! Insert(self, -1, elem)
      Future.sequence(subtrees.values.map(_ ask x)).foreach(_ => {
        send ! CopyFinished
        context.stop(self)
      })

  }

  def mkChild(elem: Int) = context.actorOf(props(elem, false))

  def insertReceive: Receive = {
    case x: Insert if x.elem == elem =>
      if(isRemove) {
        isRemove = false
      }
      x.requester ! OperationFinished(x.id)
    case x: Insert if x.elem < elem  =>
      if(subtrees.contains(Left))
        subtrees(Left) ! x
      else {
        subtrees += Left -> mkChild(x.elem)
        x.requester ! OperationFinished(x.id)
      }
    case x: Insert if x.elem > elem  =>
      if(subtrees.contains(Right))
        subtrees(Right) ! x
      else {
        subtrees += Right -> mkChild(x.elem)
        x.requester ! OperationFinished(x.id)
      }
  }

  def containReceive: Receive = {
    case x: Contains if x.elem == elem                            =>
      if(isRemove)
        x.requester ! ContainsResult(x.id, false)
      else
        x.requester ! ContainsResult(x.id, true)
    case x: Contains if x.elem < elem && subtrees.contains(Left)  =>
      subtrees(Left) ! x
    case x: Contains if x.elem > elem && subtrees.contains(Right) =>
      subtrees(Right) ! x
    case x: Contains                                              =>
      x.requester ! ContainsResult(x.id, false)
  }

  def removeReceive: Receive = {
    case x: Remove if x.elem == elem                             =>

      x.requester ! OperationFinished(x.id)
      isRemove = true
    case x: Remove if x.elem < elem && subtrees.contains(Left)   =>
      subtrees(Left) ! x
    case x: Remove if x.elem >= elem && subtrees.contains(Right) =>
      subtrees(Right) ! x
    case x: Remove                                               =>
      x.requester ! OperationFinished(x.id)
  }

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = copyReceive orElse insertReceive orElse containReceive orElse removeReceive

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???


}
