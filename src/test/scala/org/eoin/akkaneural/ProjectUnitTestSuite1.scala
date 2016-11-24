package org.eoin.akkaneural

import java.util.{Random => JRandom}

import akka.actor.Props
import akka.testkit.{TestActorRef, TestKit}
import org.eoin._
import org.junit.{Ignore, Test}
import org.scalacheck.Gen
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import spire.math.Real

import scala.concurrent.duration._


trait TestConfig extends InjectableConfig {
  override def getRealNumberRNG: () => Real = () => 0.5
  override def getActivationFunction: (Real) => Real = identity[Real]
}

class ProjectUnitTestSuite1 extends JUnitSuite with GeneratorDrivenPropertyChecks with TestConfig{

  @Test def testHelloWorld : Unit = {

    val gen = Gen.oneOf("Hello", "World!")
    forAll (gen, minSuccessful(5)) { (s: String) =>
      logger.info(s)
    }
  }

  //manual re running
  @Ignore @Test def testProblemCases : Unit = {

    performRun(List(7, 1, 7, 8, 8, 9, 7, 3, 6, 4, 7) ,20 )

  }



  @Test def testCorrectOutputs : Unit = {

    val layerSizesGenerator: Gen[List[Int]] = for {
      inputLayerSize <- Gen.choose[Int](1,50)
      hiddenLayerSizes <- Gen.containerOf[List, Int](Gen.choose(1,100))
      outputLayerSize <- Gen.choose[Int](1,50)
    } yield (inputLayerSize :: hiddenLayerSizes) :+ outputLayerSize

    forAll (layerSizesGenerator,minSuccessful(10)) { (layerSizes : List[Int]) =>
      whenever (layerSizes.length > 1 && ! layerSizes.exists( _ <= 0)) {

        performRun(layerSizes, 200)

      }
    }
  }


  private def performRun(layerSizes: List[Int], numRowsOfData : Int) : Unit = {
      logger.info(s" *** performRun layerSizes : ${layerSizes} numRowsOfData : ${numRowsOfData}")

      val neuralNet = new NeuralNet(layerSizes,
        numRowsOfData, "TestNeuralNetwork") with TestConfig

      val testActor = TestActorRef(Props(new NetworkExitPoint(numRowsOfData))) (neuralNet.actorSystem)
      neuralNet.outputRouter ! NeuronAdded(testActor, true)
      testActor ! NeuronAdded(neuralNet.outputRouter, false)

      val rng = new JRandom
      val dummyData = List.fill(neuralNet.dataRowSize * numRowsOfData){ Real(rng.nextDouble()) }
        .grouped(neuralNet.dataRowSize)

      dummyData foreach { neuralNet.entryPoint ! FeedForwardInput(_) }

      val outputData = testActor.underlyingActor.asInstanceOf[NetworkExitPoint].feedForwardInputsReceived
      TestKit.awaitCond (outputData.size == numRowsOfData, 60 seconds)

      val outputLayerSize = neuralNet.outputLayer.length
      outputData foreach { case(_,ffi:FeedForwardInput) =>
        ffi.values.length should equal(outputLayerSize + 1)} // +1 cos the bias term is included
      logger.info(s"\n\n *** outputData : ${outputData}\n\n")

      neuralNet.actorSystem.terminate()

  }


}
