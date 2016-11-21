package org.eoin.akkaneural

import akka.actor.{Actor, ActorRef, Stash}
import akka.routing.{BroadcastRoutingLogic, Router}
import org.eoin.LoggingToUse
import spire.implicits._
import spire.math._

import scala.collection.mutable.ArrayBuffer

sealed trait NeuronMessage
case class FeedForwardInput(values: List[Real]) extends NeuronMessage
case class FeedForwardOutput(value: Real) extends NeuronMessage
case class BackPropagationInput(delta: Real) extends NeuronMessage
case class NeuronAdded(ref: ActorRef, downstream: Boolean) extends NeuronMessage
case class NeuronRemoved(ref: ActorRef) extends NeuronMessage


class Neuron ( val interLayerRouter: ActorRef, /*val myIndex: Int, val myLayerIndex : Int, */
               val activationFn: Real=>Real) extends Actor with Stash with LoggingToUse {

  // TODO vars - context.become
  var biasTerm: Real = 1/2
  var inputWeights = ArrayBuffer(biasTerm) // Map.empty[ActorRef, Real]

  //var routerToNextLayer = Router(BroadcastRoutingLogic(), Vector.empty)


  override def receive: Receive = {

    //case NeuronConnected(actorRef) => routerToNextLayer = routerToNextLayer.addRoutee(actorRef)
    //case NeuronDisconnected(actorRef) => routerToNextLayer = routerToNextLayer.removeRoutee(actorRef)

    case FeedForwardInput (values) =>
      if (values.length > inputWeights.length) {      //expand array if necc, with mean of
        inputWeights ++= List.fill(values.length - inputWeights.length) (inputWeights.qmean)
      }
      val dotProduct = values.zip(inputWeights) map { r => r._1 * r._2 }
      val output = activationFn(dotProduct.qsum)

      interLayerRouter ! FeedForwardOutput(output)

    case BackPropagationInput(delta: Real) => ???

    case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x") // TODO which one
      //logger.error(s"unrecognized message $x")

  }


}

class InterLayerRouter ( val mySuccessiveLayerIndex : Int) extends Actor with Stash with LoggingToUse {

  var routerToNextLayer = Router(BroadcastRoutingLogic(), Vector.empty) // TODO var
  var previousLayerNeurons = List.empty[ActorRef]

  override def receive = handle(List.empty)


  def handle(feedForwardOutputsReceived: List[ (ActorRef,Real)] ): Receive = {

    case NeuronAdded(actorRef,true) => routerToNextLayer = routerToNextLayer.addRoutee(actorRef)
    case NeuronAdded(actorRef,false) => previousLayerNeurons = actorRef :: previousLayerNeurons
    case NeuronRemoved(actorRef) =>
      routerToNextLayer = routerToNextLayer.removeRoutee(actorRef)
      previousLayerNeurons = previousLayerNeurons.filterNot { _ == actorRef }

    case FeedForwardOutput(value) =>
      val allFeedForwards = (sender(),  value) :: feedForwardOutputsReceived
      // if we've gotten them all, send them on
      if (allFeedForwards.map {_._1} == previousLayerNeurons) { // TODO bit clunky
        routerToNextLayer.route(FeedForwardInput(allFeedForwards.map {_._2}), context.self)
        context.become(handle(List.empty))
      } else {
        context.become(handle(allFeedForwards))
      }

    case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x") // TODO which one
      //logger.error(s"unrecognized message $x")  }
  }
}

class NetworkEntryPoint extends Actor with Stash with LoggingToUse {

  var inputLayerNeurons = List.empty[ActorRef]

  override def receive : Receive = {
    case NeuronAdded(actorRef,true) => inputLayerNeurons = actorRef :: inputLayerNeurons
    case FeedForwardInput (values) =>
      require (inputLayerNeurons.size == values.size) //TODO
      inputLayerNeurons zip values foreach { case (neuron,value) => neuron ! FeedForwardInput(List(value))}
    case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x") // TODO which one
      //logger.error(s"unrecognized message $x")
  }

}

class NetworkExitPoint extends Actor with Stash with LoggingToUse {

   var outputLayerNeurons = List.empty[ActorRef]

  override def receive : Receive = {
    case NeuronAdded(actorRef,false) => outputLayerNeurons = actorRef :: outputLayerNeurons
    case f @ FeedForwardInput (values) =>
      require (outputLayerNeurons.size == values.size) //TODO
      log.info(f.toString)
     case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x") // TODO which one
      //logger.error(s"unrecognized message $x")
    }

}
