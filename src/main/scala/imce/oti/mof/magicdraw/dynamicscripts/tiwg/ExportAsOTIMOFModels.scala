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

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node => MDNode, Tree => MDTree}
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.uml2.ext.jmi.helpers.StereotypesHelper
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}
import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{None, Option, Some, StringContext, Tuple2, Tuple3, Tuple4}
import scala.Predef.{ArrowAssoc, String}
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

  import Utils.{IterableSemigroup,VectorSemigroup,selectable}

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
    exportAsOTIMOFModel,
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
    exportAsOTIMOFModel,
    Utils.chooseOTIDocumentSetConfigurationForUserModel)

  def exportAsOTIMOFModel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[Document[MagicDrawUML] => \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent]]
  = resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
    case Some(pt: OTIMOFLibraryResourceExtent) =>
      resourceExtents.find(Utils.UML25_IRI == _.resource.iri) match {
        case Some(mm: OTIMOFMetamodelResourceExtent) =>
          import Utils.selectable
          Success(
            exportAsOTIMOFModel(
              p, odsa,
              pt, mm,
              resourceExtents.select { case pfExt: OTIMOFProfileResourceExtent => pfExt }))
        case _ =>
          Failure(new java.lang.IllegalArgumentException("No MD18 UML2.5 metamodel resource found!"))
      }
    case _ =>
      Failure(new java.lang.IllegalArgumentException("No MD18 PrimitiveTypes library resource found!"))
  }

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
  (modelIRI: String @@ Identification.ModelIRI,
   allAppliedStereotypesByOptionalProfile: AppliedStereotypesByOptionalProfile,
   profiles: Set[OTIMOFProfileResourceExtent])
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
  (applyingModel: String @@ Identification.ModelIRI,
   pf: UMLProfile[MagicDrawUML],
   profiles: Set[OTIMOFProfileResourceExtent])
  : Vector[java.lang.Throwable] \&/ OTIMOFProfileResourceExtent
  = pf.URI match {
    case None =>
      \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLProfile[MagicDrawUML]](
        s"Cannot lookup OTI MOF Profile resource without a Profile::URI for ${pf.qualifiedName.get}",
        Iterable(pf))))
    case Some(pfURI) =>
      profiles
        .find { r => pfURI == Identification.ProfileIRI.unwrap(r.resource.iri) } match {
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
        val sUUID = Identification.StereotypeUUID(TOOL_SPECIFIC_UUID.unwrap(s.toolSpecific_uuid.get))

        val inc
        : Vector[java.lang.Throwable] \&/ Vector[model.AppliedStereotype]
        = pfR
          .classifiers
          .find(sUUID == _.uuid) match {
          case None =>
            \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLStereotype[MagicDrawUML]](
              s"Unknown stereotype '${s.qualifiedName.get}' applied to ${es.size} elements",
              Iterable(s)
            )))
          case Some(otiS) =>
            \&/.That(es.map { e =>
              model.AppliedStereotype(
                modelElement = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
                appliedStereotype = otiS.uuid)
            }
              .toVector)
        }

        acc append inc
    },
    _ append _)

  def exportAsOTIMOFModel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    primitiveTypesR: OTIMOFLibraryResourceExtent,
    umlR: OTIMOFMetamodelResourceExtent,
    profiles: Set[OTIMOFProfileResourceExtent] )
  ( d: Document[MagicDrawUML] )
  : Vector[java.lang.Throwable] \&/ OTIMOFResourceExtent
  = for {
    allAppliedStereotypesByOptionalProfile <- d.scope.allAppliedStereotypesByProfile match {
      case -\/(errors) =>
        \&/.This(errors.toVector)
      case \/-(result) =>
        \&/.That(result)
    }

    modelIRI = Identification.ModelIRI(OTI_URI.unwrap(d.info.packageURI))

    umlResolver = UMLMetamodelResolver.initialize(primitiveTypesR, umlR)

    allAppliedStereotypesByProfile <-
    onlyAppliedStereotypesByProfile(modelIRI, allAppliedStereotypesByOptionalProfile, profiles)

    appliedStereotypes <- allAppliedStereotypesByProfile
      .aggregate[\&/[Vector[java.lang.Throwable], Vector[model.AppliedStereotype]]](\&/.That(Vector()))(
      {
        case (acc, (_, pfR, _, stereotypedElements)) =>
          import Utils.VectorSemigroup
          acc append toAppliedStereotypes(pfR, stereotypedElements)
      },
      _ append _)

    result <- exportAsOTIMOFModelExtent(
      p, odsa,
      umlResolver, profiles,
      d, modelIRI, appliedStereotypes, allAppliedStereotypesByProfile)

  } yield result

  def exportAsOTIMOFModelExtent
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    umlResolver: UMLMetamodelResolver,
    profiles: Set[OTIMOFProfileResourceExtent],
    d: Document[MagicDrawUML],
    modelIRI: String @@ Identification.ModelIRI,
    appliedStereotypes: Vector[model.AppliedStereotype],
    allAppliedStereotypesByProfile: AppliedStereotypesByProfile)
  : Vector[java.lang.Throwable] \&/ OTIMOFResourceExtent
  = {
    implicit val ops = odsa.otiAdapter.umlOps

    val ch = odsa.otiAdapter.otiCharacteristicsProvider

    def elementAttributeSeqOp
    ( acc: Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue],
      e: MagicDrawUMLElement )
    : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
    = acc append toModelElementAttributeValues(e, ch, umlResolver)

    val pExtent = d.extent match {
      case ex: Set[UMLElement[MagicDrawUML]] =>
        ex.map(ops.umlMagicDrawUMLElement).par
      case ex =>
        ex.map(ops.umlMagicDrawUMLElement).toSet.par
    }

    pExtent.tasksupport =
      new scala.collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(Utils.poolSize))

    val elements
    : Vector[model.ModelElement]
    = pExtent.map(toModelElement).toVector

    val links
    : Vector[model.ModelLink]
    = Vector()

    val appliedStereotypePropertyReferences
    : Vector[model.AppliedStereotypePropertyReference]
    = Vector()

    val elementAttributeValues
    : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
    = pExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]](\&/.That(Vector()))(
      elementAttributeSeqOp, _ append _)

    val instantiatedMetamodels
    : Vector[OTIMOFResourceInstantiatedMetamodel]
    = Vector()

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

    guiLog.log(s"Extent: ${d.info.packageURI}")

    elementAttributeValues.onlyThis match {
      case None =>
        \&/.That(extent)
      case Some(errors) =>
        \&/.Both(errors, extent)
    }
  }

  def toModelElement
  (e: UMLElement[MagicDrawUML])
  : model.ModelElement
  = model.ModelElement(
    uuid = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
    metaClass = Identification.MetaClassUUID(e.mofMetaclassName))

  def toModelElementAttributeValues
  (e: MagicDrawUMLElement,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML],
   umlResolver: UMLMetamodelResolver)
  : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = umlResolver.mc2AllAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeUnorderedProperty])

    mdMC.getEAllAttributes.aggregate[Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]](
      \&/.That(Vector())
    )(
      {
        case (acc, ea) if !ea.isDerived =>
          val name = ea.getName

          attribs
            .find { p =>
              name == Common.Name.unwrap(p.name)
            } match {

            case None =>
              acc

            case Some(attrib) =>
              if (!mdE.eIsSet(ea))
                acc
              else {
                // @todo it seems that MD UML Ecore metamodel does not have default values...
                val default = ea.getDefaultValue

                if (ea.isOrdered) {
                  val attributeValues = mdE.eGet(ea) match {
                    case vs: java.util.Collection[_] =>
                      vs
                        .zipWithIndex
                        .map { case (v, i) =>
                          model.ModelElementOrderedAttributeValue(
                            modelElement = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = Identification.AtomicValueRepresentation(v.toString)),
                            index = i)
                        }
                        .toVector
                    case v =>
                      Vector(
                        model.ModelElementOrderedAttributeValue(
                          modelElement = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
                          attributeValue = values.AtomicValue(
                            attribute = attrib.uuid,
                            value = Identification.AtomicValueRepresentation(v.toString)),
                          index = 0))
                  }
                  acc append \&/.That(attributeValues)
                } else {
                  val attributeValues = mdE.eGet(ea) match {
                    case vs: java.util.Collection[_] =>
                      vs
                        .map { v =>
                          model.ModelElementUnorderedAttributeValue(
                            modelElement = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = Identification.AtomicValueRepresentation(v.toString)))
                        }
                        .toVector
                    case v =>
                      if (v != default)
                        Vector(
                          model.ModelElementUnorderedAttributeValue(
                            modelElement = Identification.ModelElementUUID(TOOL_SPECIFIC_UUID.unwrap(e.toolSpecific_uuid.get)),
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = Identification.AtomicValueRepresentation(v.toString))))
                      else
                        Vector()
                  }
                  acc append \&/.That(attributeValues)
                }
              }
          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

}