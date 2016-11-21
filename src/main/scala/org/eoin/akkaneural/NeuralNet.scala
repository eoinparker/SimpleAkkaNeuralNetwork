package org.eoin.akkaneural

import akka.actor.{ActorRef, ActorSystem, Props}
import spire.math._
import spire.algebra._

/**
  * Created by eoin.parker on 11/17/16.
  */
object NeuralNet extends App {

  val actorSystem = ActorSystem("SimpleAkkaNeuralNetwork")

  val entryPoint  = actorSystem.actorOf(Props(new NetworkEntryPoint), "entryPoint")
  val inputLayerToHiddenLayer = actorSystem.actorOf(Props(new InterLayerRouter(2)), "inputLayerToHiddenLayer")
  val hiddenLayerToOutputLayer = actorSystem.actorOf(Props(new InterLayerRouter(3)), "hiddenLayerToOutputLayer")
  val exitPoint  = actorSystem.actorOf(Props(new NetworkExitPoint), "exitPoint")

  val sigmoidActivationFn = (r:Real) => 1 / (1 + Trig[Real].exp(r))

 val inputLayer = Array.tabulate(3)( i => actorSystem.actorOf(Props(new Neuron(inputLayerToHiddenLayer, sigmoidActivationFn)), s"inputLayer$i" ))
  val hiddenLayer = Array.tabulate(5)(i => actorSystem.actorOf(Props(new Neuron(hiddenLayerToOutputLayer, sigmoidActivationFn)), s"hiddenLayer$i" ))
  val outputLayer = Array.tabulate(2)(i => actorSystem.actorOf(Props(new Neuron(exitPoint, identity)), s"outputLayer$i" ))

  inputLayer foreach { a: ActorRef =>
    entryPoint ! NeuronAdded(a, true)
    inputLayerToHiddenLayer ! NeuronAdded(a, false)
  }
  hiddenLayer foreach { a: ActorRef =>
    inputLayerToHiddenLayer ! NeuronAdded(a, true)
    hiddenLayerToOutputLayer ! NeuronAdded(a, false)
  }
  outputLayer foreach { a: ActorRef =>
    hiddenLayerToOutputLayer ! NeuronAdded(a, true)
    exitPoint ! NeuronAdded(a, false)
  }

  val dummyData = List( Real(0.25), Real(-0.75), Real(0.01), Real(-0.15), Real (0.91456), Real (0.99)).grouped(3)

  dummyData foreach( entryPoint ! FeedForwardInput(_))

  Thread.sleep(100000) ;

  actorSystem.terminate()


}
