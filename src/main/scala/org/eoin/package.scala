package org

import akka.actor.DiagnosticActorLogging
import org.apache.logging.log4j.LogManager

/**
  * Created by eoin.parker on 11/10/16.
  */
package object eoin {

  val logger = LogManager.getLogger(this.getClass.getCanonicalName)

  type LoggingToUse = DiagnosticActorLogging

}
 
