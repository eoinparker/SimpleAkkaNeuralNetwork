package org.eoin.akkaneural

import akka.testkit.TestProbe
import org.eoin._
import org.junit.Test
import org.scalacheck.Gen
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import spire.math.Real

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

class ProjectUnitTestSuite1 extends JUnitSuite with GeneratorDrivenPropertyChecks {

  @Test def testHelloWorld : Unit = {

    val gen = Gen.oneOf("Hello", "World!")
    forAll (gen, minSuccessful(5)) { (s: String) =>
      logger.info(s)
    }
  }

  @Test def testCorrectOutputSize : Unit = {

    val layerSizesGenerator: Gen[List[Int]] = for {
      inputLayerSize <- Gen.choose[Int](1,4)
      hiddenLayerSizes <- Gen.containerOf[List, Int](Gen.choose(1,10))
      outputLayerSize <- Gen.choose[Int](1,5)
    } yield (inputLayerSize :: hiddenLayerSizes) :+ outputLayerSize

    //val random0to1RealGenerator = //TODO

    forAll (layerSizesGenerator, minSize(10)) { (layerSizes : List[Int]) =>
      whenever (layerSizes.length > 1 && ! layerSizes.exists( _ <= 0)) {

        logger.info(s" *** layerSizes : ${layerSizes}")
        val neuralNet = new NeuralNet(layerSizes, "TestNeuralNetwork")

        import org.eoin.rng
        val numRowsOfData = 20
        val dummyData = List.fill(neuralNet.dataRowSize * numRowsOfData){ Real(rng.nextDouble()) }.grouped(neuralNet.dataRowSize)
        dummyData foreach( neuralNet.entryPoint ! FeedForwardInput(_))
        Thread.sleep(3000) ;

        val probe = TestProbe()(neuralNet.actorSystem)
        neuralNet.actorSystem.actorSelection("akka://SimpleAkkaNeuralNetwork/user/exitPoint") ! (GetState, probe)
        val stateMsg = probe.fishForMessage(60000 millis, "GetState") { case (lbff: ListBuffer[FeedForwardInput]) => true }

        stateMsg.asInstanceOf[ListBuffer[FeedForwardInput]].size should equal(layerSizes.last)

        neuralNet.actorSystem.terminate()
      }
    }
  }


}
