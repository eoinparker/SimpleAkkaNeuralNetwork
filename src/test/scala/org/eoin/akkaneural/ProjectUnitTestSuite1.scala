package org.eoin.akkaneural

import org.junit.Test
import org.scalacheck.Gen
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.eoin._


class ProjectUnitTestSuite1 extends JUnitSuite with GeneratorDrivenPropertyChecks {

  @Test def basicTest1 : Unit = {

    val gen = Gen.oneOf("Hello", "World!")
    forAll (gen, minSuccessful(5)) { (s: String) =>
      logger.info(s)
    }
  }
}
