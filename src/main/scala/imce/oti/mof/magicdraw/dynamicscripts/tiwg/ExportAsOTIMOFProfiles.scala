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
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Profile
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.json.common._
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api.{UMLProperty, UMLStereotype}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Option, None, PartialFunction, Some, StringContext}
import scala.util.{Failure, Success, Try}
import scalaz._

/**
  * Export selected UML Profiles as OTI MOF Profiles, that is, the exported OTIMOFProfileResourceExtents
  * will only have the UML Stereotypes & stereotype associations corresponding to an OTI MOF profile,
  * which is a restricted kind of OMG UML 2.5 profile without profile-defined datatypes or classes.
  *
  * Everything outside the scope of an OTIMOFProfileResourceExtent is ignored
  */
object ExportAsOTIMOFProfiles {

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Profile,
    selection: java.util.Collection[Profile] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.browserDynamicScript(
    p, ev, script, tree, node, top, selection,
    "exportAsOTIMOFProfile",
    exportAsOTIMOFProfile,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel)

  def doit
  ( p: Project,
    ev: ActionEvent,
    script: DynamicScriptsTypes.DiagramContextMenuAction,
    dpe: DiagramPresentationElement,
    triggerView: PackageView,
    triggerElement: Profile,
    selection: java.util.Collection[PresentationElement])
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.diagramDynamicScript(
    p, ev, script, dpe, triggerView, triggerElement, selection,
    "exportAsOTIMOFProfile",
    exportAsOTIMOFProfile,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel)

  def exportAsOTIMOFProfile
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[Document[MagicDrawUML] => \/[Set[java.lang.Throwable], OTIMOFResourceExtent]]
  = resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
    case Some(pt: OTIMOFLibraryResourceExtent) =>
      resourceExtents.find(Utils.UML25_IRI == _.resource.iri) match {
        case Some(mm: OTIMOFMetamodelResourceExtent) =>
          Success(exportAsOTIMOFProfile(p, odsa, pt, mm) _)
        case _ =>
          Failure(new java.lang.IllegalArgumentException("No MD18 UML2.5 metamodel resource found!"))
      }
    case _ =>
      Failure(new java.lang.IllegalArgumentException("No MD18 PrimitiveTypes library resource found!"))
  }

  def exportAsOTIMOFProfile
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    pt: OTIMOFLibraryResourceExtent,
    umlR: OTIMOFMetamodelResourceExtent )
  ( d: Document[MagicDrawUML] )
  : \/[Set[java.lang.Throwable], OTIMOFResourceExtent]
  = {
    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val pf = OTIMOFProfile(Identification.ProfileIRI(OTI_URI.unwrap(d.info.packageURI)))

    implicit val ops = odsa.otiAdapter.umlOps
    import ops._

    val ss
    : Vector[UMLStereotype[MagicDrawUML]]
    = d
      .scope
      .ownedType
      .selectByKindOf { case mc: UMLStereotype[MagicDrawUML] => mc }
      .to[Vector]
      .sortBy(_.toolSpecific_uuid.get)

    for {
      s2mc <- getStereotype2ExtendedMetaclasses(ss, umlR)
      ext = OTIMOFProfileResourceExtent(
        resource = pf,
        classifiers = ss.map { s =>
          profile.Stereotype(
            uuid = Identification.StereotypeUUID(TOOL_SPECIFIC_UUID.unwrap(s.toolSpecific_uuid.get)),
            name = Common.Name(s.name.get))
        },
        extendedMetaclass = s2mc)
    } yield {
      guiLog.log(s"Extent: ${d.info.packageURI}")
      ext
    }
  }

  def getStereotype2ExtendedMetaclasses
  ( ss: Vector[UMLStereotype[MagicDrawUML]],
    umlR: OTIMOFMetamodelResourceExtent )
  : \/[Set[java.lang.Throwable], Vector[profile.Stereotype2ExtendedMetaclass]]
  = ss.foldLeft[\/[Set[java.lang.Throwable], Vector[profile.Stereotype2ExtendedMetaclass]]](\/-(Vector())) {
    case (acc1, s) =>
      s.baseMetaProperties.foldLeft(acc1) {
        case (acc2, baseP) =>
          import Utils.VectorSemigroup
          acc2 +++ getStereotypeBaseProperty2ExtendedMetaclass(s, baseP, umlR)
      }
  }

  def getStereotypeBaseProperty2ExtendedMetaclass
  ( s: UMLStereotype[MagicDrawUML],
    baseP: UMLProperty[MagicDrawUML],
    umlR: OTIMOFMetamodelResourceExtent )
  : \/[Set[java.lang.Throwable], Vector[profile.Stereotype2ExtendedMetaclass]]
  = baseP._type match {
    case None =>
      -\/(Set(
        UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
          s"Stereotype base property ${baseP.qualifiedName.get} should be typed by a metaclass",
          Iterable(baseP))))
    case Some(extMC) =>
      import Utils.selectable

      umlR
        .classifiers
        .select { case umlMC: metamodel.MetaClass => umlMC }
        .find { umlMC => Common.Name.unwrap(umlMC.name) == extMC.name.get } match {
        case None =>
          -\/(Set(
            UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
              s"Stereotype base property ${baseP.qualifiedName.get} refers to an unknown metaclass ${extMC.qualifiedName.get}",
              Iterable(baseP))))
        case Some(umlMC) =>
          \/-(Vector(
            profile.Stereotype2ExtendedMetaclass(
              extendingStereotype = Identification.StereotypeUUID(TOOL_SPECIFIC_UUID.unwrap(s.toolSpecific_uuid.get)),
              extendedMetaclass = umlMC.uuid)))
      }
  }

}