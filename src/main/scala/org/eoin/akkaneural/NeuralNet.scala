package org.eoin.akkaneural

import akka.actor.{ActorRef, ActorSystem, Props}
import spire.math._
import spire.algebra._

/**
  * Created by eoin.parker on 11/17/16.
  */
object NeuralNet extends App {

  val actorSystem = ActorSystem("SimpleAkkaNeuralNetwork")
  val sigmoidActivationFn = (r:Real) => 1 / (1 + Trig[Real].exp(r))


  // the layers of neurons
 val inputLayer = Array.tabulate(3)( i => actorSystem.actorOf(Props(new Neuron(sigmoidActivationFn)), s"inputLayer$i" ))
  val hiddenLayer = Array.tabulate(5)(i => actorSystem.actorOf(Props(new Neuron(sigmoidActivationFn)), s"hiddenLayer$i" ))
  val outputLayer = Array.tabulate(2)(i => actorSystem.actorOf(Props(new Neuron(/*identity*/ sigmoidActivationFn)), s"outputLayer$i" )) //TODO identity or sig

  // the routers between successive pairs of layers.  Job is to collate all previous computes & then broadcast onward
  val inputLayerToHiddenLayer = actorSystem.actorOf(Props(new InterLayerRouter(2)), "inputLayerToHiddenLayer")
  val hiddenLayerToOutputLayer = actorSystem.actorOf(Props(new InterLayerRouter(3)), "hiddenLayerToOutputLayer")
  val outputLayerToExitPoint = actorSystem.actorOf(Props(new InterLayerRouter(4)), "outputLayerToExitPoint")

  // send a dataset, row by row
  val entryPoint  = actorSystem.actorOf(Props(new NetworkEntryPoint), "entryPoint")
  // will await + collate results from output layer
  val exitPoint  = actorSystem.actorOf(Props(new NetworkExitPoint), "exitPoint")


  // hook everything up
  inputLayer foreach { a: ActorRef =>
    entryPoint ! NeuronAdded(a, true)
    inputLayerToHiddenLayer ! NeuronAdded(a, false)
    a ! NeuronAdded(inputLayerToHiddenLayer, true)
  }
  hiddenLayer foreach { a: ActorRef =>
    inputLayerToHiddenLayer ! NeuronAdded(a, true)
    hiddenLayerToOutputLayer ! NeuronAdded(a, false)
    a ! NeuronAdded(hiddenLayerToOutputLayer, true)
  }
  outputLayer foreach { a: ActorRef =>
    hiddenLayerToOutputLayer ! NeuronAdded(a, true)
    outputLayerToExitPoint ! NeuronAdded(a, false)
    a ! NeuronAdded(outputLayerToExitPoint, true)
}

  outputLayerToExitPoint ! NeuronAdded(exitPoint, true)
  exitPoint ! NeuronAdded(outputLayerToExitPoint, false)

  val dummyData = List( Real(0.25), Real(-0.75), Real(0.01), Real(-0.15), Real (0.91456), Real (0.99)).grouped(3)

  dummyData foreach( entryPoint ! FeedForwardInput(_))

  Thread.sleep(1000) ;

  actorSystem.terminate()


}
