package org.eoin.akkaneural

import akka.actor.{ActorSystem, Props}

/**
  * Created by eoin.parker on 11/17/16.
  */
class NeuralNet extends App {

  val actorSystem = ActorSystem("SimpleAkkaNeuralNetwork")

  val neuron1  = actorSystem.actorOf(Props(new Neuron(1,1)))



}
