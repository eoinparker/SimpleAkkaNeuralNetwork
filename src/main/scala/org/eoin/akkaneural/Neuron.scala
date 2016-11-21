package org.eoin.akkaneural

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import akka.event.LoggingReceive
import akka.routing.{BroadcastRoutingLogic, Router}
import spire.implicits._
import spire.math._

sealed trait NeuronMessage
case class FeedForwardInput(values: List[Real]) extends NeuronMessage
case class FeedForwardOutput(value: Real) extends NeuronMessage
case class BackPropagationInput(delta: Real) extends NeuronMessage
case class NeuronAdded(ref: ActorRef, downstream: Boolean) extends NeuronMessage
case class NeuronRemoved(ref: ActorRef) extends NeuronMessage


class Neuron ( val activationFn: Real=>Real) extends Actor with Stash with ActorLogging {

  import org.eoin.rng

  // TODO vars - change to context.become or FSM
  var biasTerm = Real(0.5)
  var inputWeights = Array(biasTerm)
  var outputDestinationNeurons = List.empty[ActorRef]

  override def receive: Receive = LoggingReceive {

    case NeuronAdded(actorRef,true) => outputDestinationNeurons = actorRef :: outputDestinationNeurons

    case FeedForwardInput (values) =>
      if (values.length > inputWeights.length ) {      //expand array if necc , with Random /or w mean of existing values //TODO clunky
        inputWeights = inputWeights ++ Array.fill(values.length - inputWeights.length) (Real(rng.next[Double]))  // (inputWeights.qmean)
      }
      val dotProduct = values.zip(inputWeights) map { r => r._1 * r._2 }
      val output = activationFn(dotProduct.qsum)
      outputDestinationNeurons foreach { _ ! FeedForwardOutput(output) }

    case BackPropagationInput(delta: Real) => ???

    case x: Any =>
      log.error(s"Neuron $this unrecognized message $x")

  }
}

class InterLayerRouter extends Actor with Stash with ActorLogging {

  var routerToNextLayer = Router(BroadcastRoutingLogic(), Vector.empty) // TODO var
  var previousLayerNeurons = List.empty[ActorRef]

  override def receive = handle(List.empty)

  def handle(feedForwardOutputsReceived: List[ (ActorRef,Real)] ): Receive = LoggingReceive {

    case NeuronAdded(actorRef,true) => routerToNextLayer = routerToNextLayer.addRoutee(actorRef)
    case NeuronAdded(actorRef,false) => previousLayerNeurons = actorRef :: previousLayerNeurons
    case NeuronRemoved(actorRef) =>
      routerToNextLayer = routerToNextLayer.removeRoutee(actorRef)
      previousLayerNeurons = previousLayerNeurons.filterNot { _ == actorRef }

    case FeedForwardOutput(value) =>
      //require(previousLayerNeurons.contains(sender()))
      val allFeedForwards = (sender(),  value) :: feedForwardOutputsReceived
      // if we've gotten an input from all upstream neurons, parcel them up & send them on
      if (allFeedForwards.map {_._1}.toSet == previousLayerNeurons.toSet) { // TODO bit clunky
        val allActivations = Real(1.0) :: allFeedForwards.map {_._2}  // 1.0 prepended for the bias term
        routerToNextLayer.route(FeedForwardInput(allActivations), context.self)
        context.become(handle(List.empty))
      } else {
        // otherwise store off & keep waiting.
        context.become(handle(allFeedForwards))
      }

    case x: Any =>
      log.error(s"InterLayerRouter $this unrecognized message $x")
  }
}

class NetworkEntryPoint extends Actor with Stash with ActorLogging {

  var inputLayerNeurons = List.empty[ActorRef]

  override def receive : Receive =  LoggingReceive {
    case NeuronAdded(actorRef,true) if ! inputLayerNeurons.contains(actorRef) => inputLayerNeurons = actorRef :: inputLayerNeurons
    case FeedForwardInput (values) =>
      require (inputLayerNeurons.size == values.size) //TODO
      inputLayerNeurons zip values foreach { case (neuron,value) => neuron ! FeedForwardInput(Real(1.0) :: List(value)) }  // 1.0 prepended for the bias term
    case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x")
  }
}

class NetworkExitPoint extends Actor with Stash with ActorLogging {

  var outputLayerNeurons = List.empty[ActorRef]

  override def receive : Receive = LoggingReceive {
    case NeuronAdded(actorRef,false) =>
      outputLayerNeurons = actorRef :: outputLayerNeurons
      log.info(s"added ${actorRef} to ${context.self}")
    case f @ FeedForwardInput (values) =>
      require (outputLayerNeurons.contains(sender()))
      log.info(s"FINAL OUTPUT FROM output neuron $sender() ${f.toString}")
     case x: Any =>
      log.error(s"NetworkExitPoint $this unrecognized message $x")
    }
}
