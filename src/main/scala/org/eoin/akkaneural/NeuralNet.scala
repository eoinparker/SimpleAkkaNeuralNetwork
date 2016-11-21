package org.eoin.akkaneural

import akka.actor.{ActorSystem, Props}
import spire.algebra._
import spire.math._

/**
  * Created by eoin.parker on 11/17/16.
  */
object NeuralNet extends App {

  val actorSystem = ActorSystem("SimpleAkkaNeuralNetwork")

  val sigmoidActivationFn = (r:Real) => 1 / (1 + Trig[Real].exp(r))

  val layerSizes = List(3,5,2)  // numLayers = layerSizes.size


  // the layers of neurons
  val layers = layerSizes.zipWithIndex map { case (size,index) =>
    Array.tabulate(size)( i => actorSystem.actorOf(Props(new Neuron(sigmoidActivationFn)), s"neuronLayer_${index}_${i}" ))
  }

  // the routers between successive pairs of layers.  Job is to collate all previous computes & then broadcast onward
  val routersAfterEachLayer =  layers.zipWithIndex map { case (layer,index) =>
    val router = actorSystem.actorOf(Props(new InterLayerRouter), s"router_downstream_of_layer_${index}")
    layer foreach { neuron =>   // hook each router to its preceding neurons
      neuron ! NeuronAdded(router, true)
      router ! NeuronAdded(neuron, false)
    }
    router
  }
  routersAfterEachLayer zip layers.tail foreach {
    case ( router, downstreamLayer) =>
      downstreamLayer foreach { downstreamNeuron =>  // hook each router to its successive neurons
        downstreamNeuron ! NeuronAdded(router, true)
        router ! NeuronAdded(downstreamNeuron, false)
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

  val exitPoint  = actorSystem.actorOf(Props(new NetworkExitPoint), "exitPoint")
  outputRouter ! NeuronAdded(exitPoint, true)
  exitPoint ! NeuronAdded(outputRouter, false)


  // TODO remove & move to test
    Thread.sleep(2000) ;
  import org.eoin.rng
  val dummyData = List.fill(layerSizes.size * 5){ Real(rng.nextDouble()) }.grouped(layerSizes.size)

  dummyData foreach( entryPoint ! FeedForwardInput(_))

  Thread.sleep(10000) ;


  actorSystem.terminate()

}
