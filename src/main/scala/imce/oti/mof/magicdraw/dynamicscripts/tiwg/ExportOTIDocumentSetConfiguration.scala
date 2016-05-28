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
package imce.oti.mof.magicdraw.dynamicscripts.tiwg

import java.awt.event.ActionEvent
import java.io.PrintWriter
import java.lang.System
import java.util.concurrent.TimeUnit

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{Element, Package}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Profile
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.ui.symbols.internal.SymbolHelper._
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML._
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.utils.OTIHelper
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.validation.OTIMagicDrawValidation
import org.omg.oti.json.common._
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.{MagicDrawOTIDocumentSetAdapterForProfileProvider, MagicDrawOTIHelper, MagicDrawOTIProfileAdapter}
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.uml._
import org.omg.oti.uml.read.api.{UMLElement, UMLPackage}
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Boolean, Int, Long, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, augmentString, refArrayOps}
import scala.util.{Failure, Success, Try}
import scalaz.Scalaz._
import scalaz._

object ExportOTIDocumentSetConfiguration {

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Profile,
    selection: java.util.Collection[Element] )
  : Try[Option[MagicDrawValidationDataResults]]
  = OTIHelper.toTry(
    MagicDrawOTIHelper.getOTIMagicDrawAdapterForProfileCharacteristics(p),
    (oa: MagicDrawOTIProfileAdapter) => {

      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()

      implicit val umlOps = oa.umlOps
      import umlOps._

      val selectedSpecificationRootPackages
      : Set[UMLPackage[Uml]]
      = selection
        .to[Set]
        .selectByKindOf { case p: Package => umlPackage(p) }

      val otiV = OTIMagicDrawValidation(p)

      doit2(p, otiV, oa, selectedSpecificationRootPackages)
    })

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Package,
    selection: java.util.Collection[Element] )
  : Try[Option[MagicDrawValidationDataResults]]
  = OTIHelper.toTry(
    MagicDrawOTIHelper.getOTIMagicDrawAdapterForProfileCharacteristics(p),
    (oa: MagicDrawOTIProfileAdapter) => {

      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()

      implicit val umlOps = oa.umlOps
      import umlOps._

      val selectedSpecificationRootPackages
      : Set[UMLPackage[Uml]]
      = selection
        .to[Set]
        .selectByKindOf { case p: Package => umlPackage(p) }
      
      val otiV = OTIMagicDrawValidation(p)

      doit2(p, otiV, oa, selectedSpecificationRootPackages)
    })

  def doit
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.DiagramContextMenuAction,
   dpe: DiagramPresentationElement,
   triggerView: PackageView,
   triggerElement: Profile,
   selection: java.util.Collection[PresentationElement])
  : Try[Option[MagicDrawValidationDataResults]]
  = doit(p, ev, script, dpe, triggerView, triggerElement, selection)

  def doit
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.DiagramContextMenuAction,
   dpe: DiagramPresentationElement,
   triggerView: PackageView,
   triggerElement: Package,
   selection: java.util.Collection[PresentationElement])
  : Try[Option[MagicDrawValidationDataResults]]
  = OTIHelper.toTry(
    MagicDrawOTIHelper.getOTIMagicDrawAdapterForProfileCharacteristics(p),
    (oa: MagicDrawOTIProfileAdapter) => {

      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()

      implicit val umlOps = oa.umlOps
      import umlOps._

      val selectedSpecificationRootPackages
      : Set[UMLPackage[Uml]]
      = selection
        .toSet
        .selectByKindOf {
          case pv: PackageView =>
            umlPackage(getPackageOfView(pv).get)
        }

      val otiV = OTIMagicDrawValidation(p)

      doit2(p, otiV, oa, selectedSpecificationRootPackages)
    })

  def doit2
  (p: Project,
   otiV: OTIMagicDrawValidation,
   oa: MagicDrawOTIProfileAdapter,
   selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]])
  : Try[Option[MagicDrawValidationDataResults]]
  = {

    implicit val umlOps = oa.umlOps

    val app = Application.getInstance()
    val guiLog = app.getGUILog
    guiLog.clearLog()

    System.out.println(s"selected packages: ${selectedSpecificationRootPackages.size}")
    selectedSpecificationRootPackages.foreach { p =>
      guiLog.log(s"- ${p.qualifiedName.get}")
    }

    val emptyConfig
    : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
    = OTIDocumentSetConfiguration.empty.that

    val t0: Long = java.lang.System.currentTimeMillis()

    val result = for {
      odsa <- MagicDrawOTIHelper.getOTIMagicDrawProfileDocumentSetAdapter(oa, selectedSpecificationRootPackages)
      t1 = java.lang.System.currentTimeMillis()
      _ = {
        System.out.println(
          s"JsonExportAsOTIDocumentSetConfiguration.getOTIMagicDrawProfileDocumentSetAdapter " +
            s"in ${prettyFiniteDuration(t1 - t0, TimeUnit.MILLISECONDS)}")
      }

      config <- (emptyConfig /: selectedSpecificationRootPackages) {
        addSpecificationRootPackage(odsa)
      }
      t2 = java.lang.System.currentTimeMillis()
      _ = {
        System.out.println(
          s"JsonExportAsOTIDocumentSetConfiguration.addSpecificationRootPackages => ${config.documents.size} documents " +
            s"in ${prettyFiniteDuration(t2 - t1, TimeUnit.MILLISECONDS)}")
      }
    } yield
      doit3(p, odsa, config, selectedSpecificationRootPackages)

    otiV.errorSet2TryOptionMDValidationDataResults(p, "*** Json Export as OTIDocumentSetConfiguration ***", result.a)

  }

  def doit3
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForProfileProvider,
   config: OTIDocumentSetConfiguration,
   selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]])
  : Unit
  = {
    implicit val ops = odsa.otiAdapter.umlOps

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val mdInstallDir = MDUML.getApplicationInstallDir.toPath
    val jsonOTIDocumentConfigurationURI = mdInstallDir.resolve(s"dynamicScripts/MagicDraw-${p.getPrimaryProjectID}.documentSet.json").toUri

    val fos = new java.io.FileOutputStream(new java.io.File(jsonOTIDocumentConfigurationURI))
    val pw = new java.io.PrintWriter(fos)

    try {

      val jconfig = Json.toJson(config)
      pw.println(Json.prettyPrint(jconfig))

      guiLog.log(s"Saved OTI DocumentSet Configuration as:")
      guiLog.log(jsonOTIDocumentConfigurationURI.toString)

    } finally {
      pw.close()
      fos.close()
    }

    ()
  }

  def addSpecificationRootPackage
  (odsa: MagicDrawOTIDocumentSetAdapterForProfileProvider)
  (ri: Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration,
   p: UMLPackage[MagicDrawUML])
  : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
  = for {
    current <- ri
    pInfo <- odsa.getSpecificationRootCharacteristics(p).toThese

  } yield pInfo.fold[OTIDocumentSetConfiguration](current) { info =>
    current.copy(
      documents =
        current.documents :+ OTIDocumentConfiguration(info, p.toolSpecific_id, p.toolSpecific_url) )
  }

}