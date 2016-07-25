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
import com.nomagic.magicdraw.ui.browser.{Node => MDNode, Tree => MDTree}
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package

import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults

import imce.oti.mof.resolvers.UMLMetamodelResolver
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache

import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}

import org.omg.oti.json.common.OTIDocumentSetConfiguration

import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{None, Option, Some, StringContext, Tuple4}
import scala.Predef.String
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._
import scalaz._

/**
  * Export selected UML Packages as OTI MOF Models based on:
  * - libraries: PrimitiveTypes
  * - metamodels: UML
  * - profiles: StandardProfile, SysML
  *
  * The intent is to make sure *everything* is accounted for in the exported OTIMOFModelResourceExtent
  * such that the original UML Packages could be re-recrated without any loss of information
  * from the OTIMOFModelResourceExtents.
  */
object ExportAsOTIMOFModels {

  import Utils.VectorSemigroup

  def doit
  ( p: Project, ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: MDTree, node: MDNode,
    top: Package,
    selection: java.util.Collection[Package] )
  : Try[Option[MagicDrawValidationDataResults]]
  = Utils.browserDynamicScript(
    p, ev, script, tree, node, top, selection,
    "exportAsOTIMOFModel",
    exportAsOTIMOFModelCallback,
    Utils.chooseOTIDocumentSetConfigurationForUserModel)

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
    exportAsOTIMOFModelCallback,
    Utils.chooseOTIDocumentSetConfigurationForUserModel)

  def exportAsOTIMOFModelCallback
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent],
    config: OTIDocumentSetConfiguration,
    selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]] )
  : Try[Option[MagicDrawValidationDataResults]]
  = for {
    cache <- resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
      case Some(primitiveTypesR: OTIMOFLibraryResourceExtent) =>
        resourceExtents.find(Utils.UML25_IRI == _.resource.iri) match {
          case Some(umlR: OTIMOFMetamodelResourceExtent) =>
            Success(MetamodelTransactionPropertyNameCache(UMLMetamodelResolver.initialize(primitiveTypesR, umlR)))
          case _ =>
            Failure(new java.lang.IllegalArgumentException("Need MagicDraw's UML OTI MOF Metamodel resource"))
        }
      case _ =>
        Failure(new java.lang.IllegalArgumentException("Need MagicDraw's PrimitiveTypes OTI MOF Library resource"))
    }
    cbpf <- ExportAsOTIMOFProfiles.exportAsOTIMOFProfile(
      p, odsa,
      resourceExtents)
    cbm = {
      implicit val c: MetamodelTransactionPropertyNameCache = cache
      exportAsOTIMOFModel(p, odsa) _
    }
    er <- Utils.exportAsOTIMOFProfiledResources(
      p, odsa, config,
      selectedSpecificationRootPackages,
      resourceExtents, cbpf, cbm, "exportAsOTIMOFModel")
  } yield er

  type AppliedStereotypesByOptionalProfile =
  Map[
    Option[UMLProfile[MagicDrawUML]],
    Map[
      (UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]),
      Set[UMLElement[MagicDrawUML]]]]

  type AppliedStereotypesByProfile =
  Vector[
    ( UMLProfile[MagicDrawUML],
      OTIMOFProfileResourceExtent,
      OTIMOFResourceModelAppliedProfile,
      Map[
        (UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]),
        Set[UMLElement[MagicDrawUML]]] )]

  def onlyAppliedStereotypesByProfile
  (modelIRI: common.ResourceIRI,
   allAppliedStereotypesByOptionalProfile: AppliedStereotypesByOptionalProfile,
   profiles: Vector[OTIMOFProfileResourceExtent])
  : Vector[java.lang.Throwable] \&/ AppliedStereotypesByProfile
  = allAppliedStereotypesByOptionalProfile
    .aggregate[\&/[Vector[java.lang.Throwable], AppliedStereotypesByProfile]](\&/.That(Vector()))(
    {
      case (acc, (optionalProfile, appliedStereotypes)) =>
        import Utils.VectorSemigroup
        if (optionalProfile.isEmpty && appliedStereotypes.nonEmpty) {
          val messages = new scala.collection.mutable.ListBuffer[String]()
          messages += s"TODO: handle stereotypes defined without a profile!"
          appliedStereotypes.foreach { case ((s, _), es) =>
            messages += s"=> ${es.size} applications of ${s.qualifiedName.get}"
          }
          acc append
            \&/.This(Vector(
              UMLError.illegalElementError[MagicDrawUML, UMLElement[MagicDrawUML]](
                messages.mkString("\n"),
                appliedStereotypes.keys.map(_._1))))
        } else {
          val pf = optionalProfile.get
          val inc
          : \&/[Vector[java.lang.Throwable], AppliedStereotypesByProfile]
          = lookupProfile(modelIRI, pf, profiles).map { pfR =>
            Vector(Tuple4(
              pf,
              pfR,
              OTIMOFResourceModelAppliedProfile(modelIRI, pfR.resource.iri),
              appliedStereotypes))
          }
          acc append inc
        }
    },
    _ append _)

  def lookupProfile
  (applyingModel: common.ResourceIRI,
   pf: UMLProfile[MagicDrawUML],
   profiles: Vector[OTIMOFProfileResourceExtent])
  : Vector[java.lang.Throwable] \&/ OTIMOFProfileResourceExtent
  = pf.URI match {
    case None =>
      \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLProfile[MagicDrawUML]](
        s"Cannot lookup OTI MOF Profile resource without a Profile::URI for ${pf.qualifiedName.get}",
        Iterable(pf))))
    case Some(pfURI) =>
      profiles
        .find { r => pfURI == r.resource.iri.value } match {
        case None =>
          \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLProfile[MagicDrawUML]](
            s"No OTI MOF Profile resource for ${pf.qualifiedName.get} with Profile::URI=$pfURI",
            Iterable(pf))))
        case Some(r) =>
          \&/.That(r)
      }
  }

  def toAppliedStereotypes
  (pfR: OTIMOFProfileResourceExtent,
   stereotypedElements: Map[(UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]), Set[UMLElement[MagicDrawUML]]])
  : Vector[java.lang.Throwable] \&/ Vector[model.AppliedStereotype]
  = stereotypedElements
    .aggregate[\&/[Vector[java.lang.Throwable], Vector[model.AppliedStereotype]]](\&/.That(Vector()))(
    {
      case (acc, ((s, _), es)) =>
        val sUUID = s.toOTIMOFEntityUUID

        val ps = pfR.classifiers.find { cs =>
          val ok = sUUID.value == cs.uuid.value
          if (ok)
            true
          else
            false
        }

        val inc
        : Vector[java.lang.Throwable] \&/ Vector[model.AppliedStereotype]
        = ps match {
          case None =>
            \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLStereotype[MagicDrawUML]](
              s"*** Unknown stereotype '${s.qualifiedName.get}' applied to ${es.size} elements",
              Iterable(s)
            )))
          case Some(otiS) =>
            \&/.That(es.map { e =>
              model.AppliedStereotype(
                modelElement = e.toOTIMOFEntityUUID,
                appliedStereotype = otiS.uuid)
            }
              .toVector)
        }

        acc append inc
    },
    _ append _)

  def exportAsOTIMOFModel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider )
  ( pfExtents: Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)],
    d: Document[MagicDrawUML],
    pkgDocuments: Set[Document[MagicDrawUML]] )
  ( implicit cache: MetamodelTransactionPropertyNameCache )
  : Vector[java.lang.Throwable] \&/ OTIMOFModelResourceExtent
  = for {
    allAppliedStereotypesByOptionalProfile <- d.scope.allAppliedStereotypesByProfile match {
      case -\/(errors) =>
        \&/.This(errors.toVector)
      case \/-(result) =>
        \&/.That(result)
    }

    modelIRI = d.toOTIMOFResourceIRI

    profiles = pfExtents.map(_._2)

    allAppliedStereotypesByProfile <-
    onlyAppliedStereotypesByProfile(modelIRI, allAppliedStereotypesByOptionalProfile, profiles)

    appliedStereotypes <- allAppliedStereotypesByProfile
      .aggregate[\&/[Vector[java.lang.Throwable], Vector[model.AppliedStereotype]]](\&/.That(Vector()))(
      {
        case (acc, (_, pfR, _, stereotypedElements)) =>
          import Utils.VectorSemigroup
          val inc = toAppliedStereotypes(pfR, stereotypedElements)
          val updated = acc append inc
          updated
      },
      _ append _)

    result <- exportAsOTIMOFModelExtent(
      p, odsa, pfExtents,
      d, modelIRI, appliedStereotypes, allAppliedStereotypesByProfile)

  } yield result

  def exportAsOTIMOFModelExtent
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    pfExtents: Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)],
    d: Document[MagicDrawUML],
    modelIRI: common.ResourceIRI,
    appliedStereotypes: Vector[model.AppliedStereotype],
    allAppliedStereotypesByProfile: AppliedStereotypesByProfile)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ OTIMOFModelResourceExtent
  = {
    implicit val ops = odsa.otiAdapter.umlOps
    val ch = odsa.otiAdapter.otiCharacteristicsProvider

    def elementAttributeSeqOp
    ( acc: Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue],
      e: MagicDrawUMLElement )
    : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
    = acc
      .append(ModelElementAttributeValueExporter.toModelElementAttributeValues(e, ch))
      .append(ModelElementAttributeValueExporter.toModelElementStereotypeAttributeValues(e, ch))

    val pExtent = d.extent match {
      case ex: Set[UMLElement[MagicDrawUML]] =>
        ex.map(ops.umlMagicDrawUMLElement)
      case ex =>
        ex.map(ops.umlMagicDrawUMLElement).toSet
    }

    val parExtent = pExtent.toVector.par
    parExtent.tasksupport =
      new scala.collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(Utils.poolSize))

    val elements
    : Vector[model.ModelElement]
    = parExtent.map(toModelElement).toVector

    val links
    : Vector[model.ModelLink]
    = parExtent.map(ops.umlMagicDrawUMLElement).flatMap(ModelLinkExporter.toModelLinks(odsa, cache)).seq

    val appliedStereotypePropertyReferences
    : Vector[model.AppliedStereotypePropertyReference]
    = parExtent.map(ops.umlMagicDrawUMLElement).flatMap(ModelStereotypeTagPropertyExporter.toTagValue(odsa, cache)).seq

    val elementAttributeValues
    : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
    = parExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]](\&/.That(Vector()))(
      elementAttributeSeqOp, _ append _)

    val instantiatedMetamodels
    : Vector[OTIMOFResourceInstantiatedMetamodel]
    = Vector(OTIMOFResourceInstantiatedMetamodel(
      instantiatedMetamodel = cache.resolver.umlR.resource.iri,
      instantiatingModel = modelIRI))

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val extent = OTIMOFModelResourceExtent(
      resource = OTIMOFModel(modelIRI),
      elements,
      links,
      appliedStereotypes,
      appliedStereotypePropertyReferences,
      elementAttributeValues.onlyThat.getOrElse(Vector()),
      instantiatedMetamodels,
      allAppliedStereotypesByProfile.map(_._3))

    guiLog.log(s"Model Extent: ${d.info.packageURI}")

    elementAttributeValues.onlyThis match {
      case None =>
        \&/.That(extent)
      case Some(errors) =>
        \&/.Both(errors, extent)
    }
  }

  def toModelElement
  (e: UMLElement[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : model.ModelElement
  = model.ModelElement(
    uuid = e.toOTIMOFEntityUUID,
    metaClass = cache.resolver.mcName2UUID(e.mofMetaclassName))


}