package org

import akka.actor.DiagnosticActorLogging
import org.apache.logging.log4j.LogManager
import spire.random.rng.Cmwc5

/**
  * Created by eoin.parker on 11/10/16.
  */
package object eoin {

  val rng = Cmwc5()

  val logger = LogManager.getLogger(this.getClass.getCanonicalName)

  type LoggingToUse = DiagnosticActorLogging

}
