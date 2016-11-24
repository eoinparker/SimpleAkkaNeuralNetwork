package org.eoin.akkaneural

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import akka.event.LoggingReceive
import akka.routing.{BroadcastRoutingLogic, Router}
import spire.implicits._
import spire.math._

import scala.collection.mutable.ListBuffer

sealed trait NeuronMessage
case class FeedForwardInput(values: List[Real]) extends NeuronMessage
case class FeedForwardOutput(value: Real) extends NeuronMessage
case class BackPropagationInput(delta: Real) extends NeuronMessage
case class NeuronAdded(ref: ActorRef, downstream: Boolean) extends NeuronMessage
case class NeuronRemoved(ref: ActorRef) extends NeuronMessage
case object BeginBackPropagation


class Neuron (val realNumberRNG: ()=>Real,  val activationFn: Real => Real ) extends Actor with Stash with ActorLogging {

  // TODO vars - change to context.become or FSM
  var biasTerm = Real(0.5)
  var inputWeights = Array(biasTerm)
  var outputDestinationNeurons = List.empty[ActorRef]

  override def receive: Receive = LoggingReceive {

    case NeuronAdded(actorRef,true) =>
      if (! (outputDestinationNeurons contains actorRef) ) outputDestinationNeurons = actorRef :: outputDestinationNeurons
    case NeuronAdded(actorRef,false) => // currently nothing to do
    case NeuronRemoved(actorRef) =>
      outputDestinationNeurons = outputDestinationNeurons.filterNot { _ == actorRef }

    case FeedForwardInput (values) =>
      if (values.length > inputWeights.length ) {
        //expand array if necc , with Random /or w mean of existing values //TODO clunky
        inputWeights = inputWeights ++ Array.fill(values.length - inputWeights.length) (realNumberRNG())  // (inputWeights.qmean)
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
    case NeuronAdded(actorRef,false) =>
      if (! (previousLayerNeurons contains actorRef) ) previousLayerNeurons = actorRef :: previousLayerNeurons
    case NeuronRemoved(actorRef) =>
      routerToNextLayer = routerToNextLayer.removeRoutee(actorRef)
      previousLayerNeurons = previousLayerNeurons.filterNot { _ == actorRef }

    case FeedForwardOutput(value) =>
      require(previousLayerNeurons.contains(sender()))
      val allFeedForwards = (sender(),  value) :: feedForwardOutputsReceived

      // if we've gotten an input from all upstream neurons, parcel them up & send them on
      // TODO bit clunky but there may be multiple messages from the same neuron in feedForwardOutputsReceived
      // And we only want the oldest from each neuron.
      val oldestFromEachNeuron = allFeedForwards.foldRight(ListBuffer.empty[(ActorRef,Real)]) {
        (tuple, list) => if (! (list exists { case (a,b) => a == tuple._1 } )) list += tuple else list }

      if (oldestFromEachNeuron.size == previousLayerNeurons.size) { // have gotten a value from every upstream neuron. Good to go with downstream transmit
        val allActivations = Real(1.0) :: oldestFromEachNeuron.map {_._2}.toList  // 1.0 prepended for the bias term
        routerToNextLayer.route(FeedForwardInput(allActivations), context.self)
        context.become( handle(allFeedForwards.dropWhile { oldestFromEachNeuron contains _ } ) )
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
    case NeuronAdded(actorRef,true) =>
      if (! (inputLayerNeurons contains actorRef)) inputLayerNeurons = actorRef :: inputLayerNeurons
    case NeuronAdded(actorRef,false) =>
      throw new IllegalArgumentException (s"{$sender()} is trying to add upstream of the network's entrypoint ${context.self}")
    case NeuronRemoved(actorRef) =>
      inputLayerNeurons = inputLayerNeurons.filterNot { _ == actorRef }

    case FeedForwardInput (values) =>
      require (inputLayerNeurons.size == values.size) //TODO
      inputLayerNeurons zip values foreach { case (neuron,value) => neuron ! FeedForwardInput(Real(1.0) :: List(value)) }  // 1.0 prepended for the bias term

    case x: Any =>
      log.error(s"NetworkEntryPoint $this unrecognized message $x")
  }
}

class NetworkExitPoint( val expectedNumRows : Int)  extends Actor with Stash with ActorLogging {

  var outputLayerNeurons = List.empty[ActorRef]  //TODO vars
  val feedForwardInputsReceived = ListBuffer[(ActorRef, FeedForwardInput)] ()

  override def receive : Receive = LoggingReceive {
    case NeuronAdded(actorRef,false) =>
      if (! (outputLayerNeurons contains actorRef)) outputLayerNeurons = actorRef :: outputLayerNeurons
    case NeuronAdded(actorRef,true) =>
      throw new IllegalArgumentException (s"{$sender()} is trying to add downstream of the network's exitpoint ${context.self}")
    case ffi @ FeedForwardInput (values) =>
      require (outputLayerNeurons.contains(sender()))
      log.debug(s"FINAL OUTPUT FROM output neuron $sender() ${ffi.toString}")
      feedForwardInputsReceived += ((sender(), ffi))
      if (expectedNumRows == feedForwardInputsReceived.size)
        self ! BeginBackPropagation

    case BeginBackPropagation =>
        // http://neuralnetworksanddeeplearning.com/chap2.html#the_backpropagation_algorithm
      //val outputError = _


    case x: Any =>
      log.error(s"NetworkExitPoint $this unrecognized message $x")
    }
}
 
