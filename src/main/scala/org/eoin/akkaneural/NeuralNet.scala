package org.eoin.akkaneural

import akka.actor.{ActorSystem, Props}
import spire.algebra._
import spire.math._
import spire.random.rng.Cmwc5

/**
  * Created by eoin.parker on 11/17/16.
  */

trait InjectableConfig {
  // want to inject dummy functions so can test against known outputs
  def getRealNumberRNG : () => Real
  def getActivationFunction: Real => Real

}

trait ProductionConfig extends InjectableConfig {
  val rng = Cmwc5()

  val sigmoidActivationFn = (r:Real) => {
    val minusR =  -r
    1 / (1 + Trig[Real].exp(minusR))
  }
  override def getRealNumberRNG: () => Real =  () => Real(rng.nextDouble())
  override def getActivationFunction: (Real) => Real = sigmoidActivationFn
}

class NeuralNet(val layerSizes: List[Int], val datasetNumRows: Int, val actorSystemName: String) {

  self: InjectableConfig =>

    val actorSystem = ActorSystem(actorSystemName)
    val dataRowSize = layerSizes.head

    // the layers of neurons
    val layers = layerSizes.zipWithIndex map { case (size,index) =>
      Array.tabulate(size)( i => actorSystem.actorOf(Props(
        new Neuron(getRealNumberRNG, getActivationFunction )),
        s"neuronLayer_${index}_${i}" ))
    }

    // the routers between successive pairs of layers.  Router's job is to collate all previous computes & then broadcast onward
    val routersAfterEachLayer =  layers.zipWithIndex map { case (layer,index) =>
      val router = actorSystem.actorOf(Props(new InterLayerRouter), s"router_downstream_of_layer_${index}")
      // hook each router to its preceding neurons
      layer foreach { neuron =>
        neuron ! NeuronAdded(router, true)
        router ! NeuronAdded(neuron, false)
      }
      router
    }
    // And also hook each router to its successive neurons
    routersAfterEachLayer zip layers.tail foreach {
      case ( router, downstreamLayer) =>
        downstreamLayer foreach { downstreamNeuron =>
          downstreamNeuron ! NeuronAdded(router, false)
          router ! NeuronAdded(downstreamNeuron, true)
        }
    }

    val inputLayer = layers.head
    val outputLayer = layers.last
    val outputRouter = routersAfterEachLayer.last

    val entryPoint  = actorSystem.actorOf(Props(new NetworkEntryPoint), "entryPoint")
    inputLayer foreach { n =>
      n ! NeuronAdded(entryPoint, false)
      entryPoint ! NeuronAdded(n, true)
    }

    val exitPoint  = actorSystem.actorOf(Props(new NetworkExitPoint(datasetNumRows)), "exitPoint")
    outputRouter ! NeuronAdded(exitPoint, true)
    exitPoint ! NeuronAdded(outputRouter, false)

}

object NeuralNet extends App {

  //val sigmoidActivationFn = (r:Real) => 1 / (1 + Trig[Real].exp(-r))

}
