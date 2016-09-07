/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package imce.oti.mof.magicdraw.dynamicscripts.transactions

import java.awt.event.ActionEvent
import java.beans.PropertyChangeEvent
import java.lang.Runnable
import java.util.Collection

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.transaction.MDTransactionManager
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Element
import com.nomagic.uml2.transaction.{TransactionCommitListener, TransactionManager}

import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults

import imce.oti.mof.magicdraw.dynamicscripts.tiwg.Utils

import org.omg.oti.mof.schema._
import org.omg.oti.uml.UMLError

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{AnyVal, Boolean, None, Option, Some, StringContext, Unit, deprecated}
import scala.Predef.String
import scala.util.{Failure, Success, Try}

@deprecated("", "")
class OTIMOFTransactionListener
( val umlMM: views.UMLMetamodelResolver,
  val eventNotifiers: Seq[(events.ModelChangedEvent, Iterable[String]) => Unit] )
extends TransactionCommitListener {

  val cache = MetamodelTransactionPropertyNameCache(umlMM)

  def transactionCommited(events : Collection[PropertyChangeEvent])
  : Runnable
  = new OTIMOFTransactionEventsAnalyzer(cache, events.toList, eventNotifiers)

}

object OTIMOFTransactionListener {

  val app = Application.getInstance()
  val guiLog = app.getGUILog

  val defaultModelChangedEventNotifiers = Seq(ModelChangeEventLogger.printModelChangedEvent _)

  implicit def ToMDTransactionManager
  (tm: TransactionManager)
  : MDTransactionManagerHelper
  = new MDTransactionManagerHelper(tm)

  class MDTransactionManagerHelper
  (val tm: TransactionManager)
  extends AnyVal {

    def addTransactionCommitListenerIncludingUndoAndRedo
    (tcl: TransactionCommitListener)
    : Unit
    = tm match {
      case mdtm: MDTransactionManager =>
        mdtm.addTransactionCommitListenerIncludingUndoAndRedo(tcl)
      case _ =>
        throw new java.lang.IllegalArgumentException(s"Need an MDTransactionCommit helper")
    }

    def hasOTIMOFTransactionListener()
    : Boolean
    = tm.getListeners.exists { tcl =>
      val tclName = tcl.getClass.getName
      "imce.oti.mof.magicdraw.dynamicscripts.transactions.OTIMOFTransactionListener" == tclName
    }

    def removeOTIMOFTransactionListeners()
    : Unit
    = tm.getListeners
      .flatMap { tcl =>
        val tclName = tcl.getClass.getName
        val isOTIMOFTCL = "imce.oti.mof.magicdraw.dynamicscripts.transactions.OTIMOFTransactionListener" == tclName
        if (isOTIMOFTCL)
          Some(tcl)
        else
          None
      }
      .foreach { tcl =>
        tm.removeTransactionCommitListener(tcl)
      }

  }

  def registerListener
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Element,
    selection: java.util.Collection[Element] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.choosePrimitiveTypesAndUMLMetamodel4Resolver().flatMap {
    case None =>
      Failure(UMLError.umlAdaptationError("There should be a PrimitiveTypes & UML Metamodel resolved"))

    case Some(ummR) =>
      val tm = p.getRepository.getTransactionManager
      guiLog.clearLog()
      if (tm.hasOTIMOFTransactionListener) {
        guiLog.log("Already registered...")
      } else {
        tm.addTransactionCommitListenerIncludingUndoAndRedo(
          new OTIMOFTransactionListener(ummR, defaultModelChangedEventNotifiers))
        guiLog.log("Newly registered...")
      }

      if (tm.hasOTIMOFTransactionListener)
        Success(None)
      else
        Failure(UMLError.umlAdaptationError("There should be a registered OTI/MOF TransactionListener"))
  }

  def unregisterListener
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Element,
    selection: java.util.Collection[Element] )
  : Try[Option[MagicDrawValidationDataResults]]
  = {
    val tm = p.getRepository.getTransactionManager
    guiLog.clearLog()
    if (tm.hasOTIMOFTransactionListener) {
      tm.removeOTIMOFTransactionListeners()
      guiLog.log("Newly unregistered...")
    } else {
      guiLog.log("Already unregistered...")
    }

    if (tm.hasOTIMOFTransactionListener)
      Failure(UMLError.umlAdaptationError("There should not be a registered OTI/MOF TransactionListener"))
    else
      Success(None)
  }

}