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

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}
import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api.UMLElement
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Option, StringContext}
import scala.util.Try
import scalaz._

object ExportAsOTIMOFModels {

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Package,
    selection: java.util.Collection[Package] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.browserDynamicScript(
    p, ev, script, tree, node, top, selection,
    "exportAsOTIMOFModel",
    exportAsOTIMOFModel)

  def doit
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.DiagramContextMenuAction,
   dpe: DiagramPresentationElement,
   triggerView: PackageView,
   triggerElement: Package,
   selection: java.util.Collection[PresentationElement])
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.diagramDynamicScript(
    p, ev, script, dpe, triggerView, triggerElement, selection,
    "exportAsOTIMOFModel",
    exportAsOTIMOFModel)

  def exportAsOTIMOFModel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider)
  : Document[MagicDrawUML] => OTIMOFResourceExtent
  = {
    val jHelper = OTIJsonSerializationHelper(odsa)
    implicit val ops = odsa.otiAdapter.umlOps

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    (d: Document[MagicDrawUML]) => {

      val pExtent = d.extent match {
        case ex: Set[UMLElement[MagicDrawUML]] =>
          ex.par
        case ex =>
          ex.toSet.par
      }

      val elements
      : Vector[model.ModelElement]
      = pExtent.map(toModelElement).toVector

      val app = Application.getInstance()
      val guiLog = app.getGUILog

      val m = OTIMOFModel(Identification.ModelIRI(OTI_URI.unwrap(d.info.packageURI)))

      val extent = OTIMOFModelResourceExtent(
        resource=m,
        elements
      )

      guiLog.log(s"Extent: ${d.info.packageURI}")

      extent
    }
  }

  def toModelElement
  (e: UMLElement[MagicDrawUML])
  : model.ModelElement
  = model.ModelElement(
    uuid = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
    metaClass = Identification.MetaClassUUID(e.mofMetaclassName))

}