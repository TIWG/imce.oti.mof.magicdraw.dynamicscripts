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
import java.lang.System
import java.util.concurrent.TimeUnit

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.utils.OTIHelper
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.validation.OTIMagicDrawValidation
import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.json.common.OTIDocumentSetConfiguration
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.{MagicDrawOTIDocumentSetAdapterForDataProvider, MagicDrawOTIHelper}
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLUtil}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api.{UMLElement, UMLPackage, UMLPrimitiveType}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Boolean, Int, Long, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, augmentString, refArrayOps}
import scala.util.{Failure, Success, Try}
import scalaz.Scalaz._
import scalaz._

/**
  * Export selected UML Packages as OTI MOF Libraries, that is, the exported OTIMOFLibraryResourceExtents
  * will only have the UML DataTypes defined in the selected packages.
  *
  * Everything outside the scope of an OTIMOFLibraryResourceExtent is ignored
  */
object ExportAsOTIMOFLibraries {

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Package,
    selection: java.util.Collection[Package] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.browserDynamicScript(
    p, ev, script, tree, node, top, selection,
    "exportAsOTIMOFLibrary",
    exportAsOTIMOFLibraryCallback,
    Utils.chooseOTIDocumentSetConfigurationNoResources)

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
    "exportAsOTIMOFLibrary",
    exportAsOTIMOFLibraryCallback,
    Utils.chooseOTIDocumentSetConfigurationNoResources)

  def exportAsOTIMOFLibraryCallback
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent],
    config: OTIDocumentSetConfiguration,
    selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]] )
  : Try[Option[MagicDrawValidationDataResults]]
  = for {
    cb <- exportAsOTIMOFLibrary(p, odsa, resourceExtents)
    er <- Utils.exportAsOTIMOFResource(
      p, odsa, config,
      selectedSpecificationRootPackages,
      resourceExtents, cb, "exportAsOTIMOFLibrary")
  } yield er

  def exportAsOTIMOFLibrary
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[Document[MagicDrawUML] => \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent]]
  = {
    val jHelper = OTIJsonSerializationHelper(odsa)
    implicit val ops = odsa.otiAdapter.umlOps

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    Success((d: Document[MagicDrawUML]) => {

      val pExtent = d.extent match {
        case ex: Set[UMLElement[MagicDrawUML]] =>
          ex.par
        case ex =>
          ex.toSet.par
      }

      val app = Application.getInstance()
      val guiLog = app.getGUILog

      val lib = OTIMOFLibrary(common.LibraryIRI(OTI_URI.unwrap(d.info.packageURI)))

      val extent = OTIMOFLibraryResourceExtent(
        resource=lib,
        classifiers=pExtent.flatMap(toDatatypeClassifier(_)).toVector.sortBy(_.name)
      )

      guiLog.log(s"Extent: ${d.info.packageURI}")

      \&/.That(extent)
    })
  }

  val primitiveTypeMap
  : Map[String, common.DatatypeAbbrevIRI]
  = Map(
    "Boolean" -> common.DatatypeAbbrevIRI("xsd:boolean"),
    "Integer" -> common.DatatypeAbbrevIRI("xsd:integer"),
    "Real" -> common.DatatypeAbbrevIRI("xsd:double"),
    "String" -> common.DatatypeAbbrevIRI("xsd:string"),
    "UnlimitedNatural" -> common.DatatypeAbbrevIRI("xsd:string")
    )

  def toDatatypeClassifier
  (e: UMLElement[MagicDrawUML])
  (implicit ops: MagicDrawUMLUtil)
  : Option[library.DatatypeClassifier]
  = e match {
    case pt: UMLPrimitiveType[MagicDrawUML] =>
      library.PrimitiveDataType(
        uuid = common.LibraryPrimitiveTypeUUID(TOOL_SPECIFIC_UUID.unwrap(pt.toolSpecific_uuid.get)),
        name = common.Name(pt.name.get),
        datatypeMapDefinition = primitiveTypeMap(pt.name.get)
      ).some
    case _ =>
      None
  }

}