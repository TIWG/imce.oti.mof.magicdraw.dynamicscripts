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
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Profile
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.utils.OTIHelper
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.validation.OTIMagicDrawValidation
import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.io.{Codec, Source}
import scala.{Boolean, Int, Long, None, Option, Some, StringContext, Unit}
import scala.Predef.{augmentString, refArrayOps, require, ArrowAssoc, String}
import scala.util.{Failure, Success, Try}
import scalaz._

/**
  * Export selected UML Packages as OTI MOF Metamodels, that is, the exported OTIMOFMetamodelResourceExtents
  * will only have the UML Classes & Associations corresponding to an OTI MOF metamodel, which is a restricted
  * kind of OMG MOF 2.5.1 CMOF metamodel.
  *
  * Everything outside the scope of an OTIMOFMetamodelResourceExtent is ignored
  */
object ExportAsOTIMOFMetamodels {

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Package,
    selection: java.util.Collection[Package] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.browserDynamicScript(
    p, ev, script, tree, node, top, selection,
    "exportAsOTIMOFMetamodel",
    exportAsOTIMOFMetamodel,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypes)

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
    "exportAsOTIMOFMetamodel",
    exportAsOTIMOFMetamodel,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypes)

  def exportAsOTIMOFMetamodel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[Document[MagicDrawUML] => \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent]]
  = resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
    case Some(r: OTIMOFLibraryResourceExtent) =>
      Success(exportAsOTIMOFMetamodel(p, odsa, r) _)
    case _ =>
      Failure(new java.lang.IllegalArgumentException("No MD18 PrimitiveTypes library resource found!"))
  }

  def exportAsOTIMOFMetamodel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    primitiveTypes: OTIMOFLibraryResourceExtent)
  ( d: Document[MagicDrawUML] )
  : Vector[java.lang.Throwable] \&/ OTIMOFResourceExtent
  = {
    implicit val ops = odsa.otiAdapter.umlOps
    import Utils.selectable

    val mcs
    : Vector[UMLClass[MagicDrawUML]]
    = d
      .scope
      .allOwnedElements
      .select { case mc: UMLClass[MagicDrawUML] => mc }
      .to[Vector]
      .sortBy(_.name.get)

    val mas
    : Vector[UMLAssociation[MagicDrawUML]]
    = d
      .scope
      .allOwnedElements
      .select { case mc: UMLAssociation[MagicDrawUML] => mc }
      .to[Vector]
      .sortBy(_.name.get)

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val dataTypedAttributes
    : Vector[(String @@ Identification.MetaClassUUID, UMLProperty[MagicDrawUML], Int)]
    = mcs
      .flatMap { mc =>
        val mcUUID = Identification.MetaClassUUID(TOOL_SPECIFIC_UUID.unwrap(mc.toolSpecific_uuid.get))
        mc
          .ownedAttribute
          .zipWithIndex
          .filter(_._1._type match {
            case Some(_: UMLPrimitiveType[MagicDrawUML]) =>
              true
            case Some(_: UMLEnumeration[MagicDrawUML]) =>
              true
            case _ =>
              false
          })
          .map { case (p, i) =>
            (mcUUID, p, i)
          }
      }

    val extent = OTIMOFMetamodelResourceExtent(
      resource = OTIMOFMetamodel(Identification.MetamodelIRI(OTI_URI.unwrap(d.info.packageURI))),
      classifiers =
        mcs.map { mc =>
          metamodel.MetaClass(
            uuid = Identification.MetaClassUUID(TOOL_SPECIFIC_UUID.unwrap(mc.toolSpecific_uuid.get)),
            name = Common.Name(mc.name.get))
        } ++
          mas.map { ma =>
            metamodel.MetaAssociation(
              uuid = Identification.MetaAssociationUUID(TOOL_SPECIFIC_UUID.unwrap(ma.toolSpecific_uuid.get)),
              name = Common.Name(ma.name.get))
          },
      associationEnds = mas.flatMap { ma =>
        val ends = ma.getDirectedAssociationEnd
        require(ends.isDefined, ma.qualifiedName.get)
        val (source, target) = ends.get

        Vector(
          features.AssociationSourceEnd(
            uuid = Identification.AssociationSourceEndUUID(TOOL_SPECIFIC_UUID.unwrap(source.toolSpecific_uuid.get)),
            name = Common.Name(source.name.get)),
          if (target.isComposite)
            features.AssociationTargetCompositeEnd(
              uuid = Identification.AssociationTargetEndUUID(TOOL_SPECIFIC_UUID.unwrap(target.toolSpecific_uuid.get)),
              name = Common.Name(target.name.get))
          else
            features.AssociationTargetReferenceEnd(
              uuid = Identification.AssociationTargetEndUUID(TOOL_SPECIFIC_UUID.unwrap(target.toolSpecific_uuid.get)),
              name = Common.Name(target.name.get))
        )
      },
      association2source = mas.map { ma =>
        val ends = ma.getDirectedAssociationEnd
        require(ends.isDefined, ma.qualifiedName.get)
        val (source, _) = ends.get
        metamodel.MetaAssociation2SourceEndProperty(
          association = Identification.MetaAssociationUUID(TOOL_SPECIFIC_UUID.unwrap(ma.toolSpecific_uuid.get)),
          sourceEnd = Identification.AssociationSourceEndUUID(TOOL_SPECIFIC_UUID.unwrap(source.toolSpecific_uuid.get))
        )
      },
      association2Target = mas.map { ma =>
        val ends = ma.getDirectedAssociationEnd
        require(ends.isDefined, ma.qualifiedName.get)
        val (_, target) = ends.get
        metamodel.MetaAssociation2TargetEndProperty(
          association = Identification.MetaAssociationUUID(TOOL_SPECIFIC_UUID.unwrap(ma.toolSpecific_uuid.get)),
          targetEnd = Identification.AssociationTargetEndUUID(TOOL_SPECIFIC_UUID.unwrap(target.toolSpecific_uuid.get))
        )
      },
      attributes = dataTypedAttributes.map { case (mcUUID, p, _) =>
        features.DataTypedAttributeUnorderedProperty(
          uuid = Identification.DatatypedAttributePropertyUUID(TOOL_SPECIFIC_UUID.unwrap(p.toolSpecific_uuid.get)),
          name = Common.Name(p.name.get))
      },
      metaclass2attribute = dataTypedAttributes.map { case (mcUUID, p, i) =>
          metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = Identification.DatatypedAttributePropertyUUID(TOOL_SPECIFIC_UUID.unwrap(p.toolSpecific_uuid.get)),
            index = i)
      },
      // @todo add association generalizations
      generalizations = mcs.flatMap { mc =>
        val specific = Identification.MetamodelClassifierUUID(TOOL_SPECIFIC_UUID.unwrap(mc.toolSpecific_uuid.get))
        mc.parents.flatMap {
          case pc: UMLClass[MagicDrawUML] if mcs.contains(pc) =>
            Some(metamodel.MetaClassifierGeneralization(
              specific,
              general=Identification.MetamodelClassifierUUID(TOOL_SPECIFIC_UUID.unwrap(pc.toolSpecific_uuid.get))))
          case _ =>
            None
        }
      }
    )

    guiLog.log(s"Extent: ${d.info.packageURI}")

    \&/.That(extent)
  }
}