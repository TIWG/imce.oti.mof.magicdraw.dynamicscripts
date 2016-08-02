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

import com.nomagic.magicdraw.core.{Application, Project}
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}
import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{None, Option, Some, StringContext, Tuple3}
import scala.Predef.String
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


  type AppliedStereotypesByOptionalProfile =
  Map[
    Option[UMLProfile[MagicDrawUML]],
    Map[
      (UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]),
      Set[UMLElement[MagicDrawUML]]]]

  type AppliedStereotypesByProfile =
  Vector[
    ( UMLProfile[MagicDrawUML],
      OTIMOFProfileTables,
      Map[
        (UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]),
        Set[UMLElement[MagicDrawUML]]] )]

  def onlyAppliedStereotypesByProfile
  (odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   modelIRI: common.ResourceIRI,
   allAppliedStereotypesByOptionalProfile: AppliedStereotypesByOptionalProfile,
   profiles: Vector[OTIMOFProfileTables])
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
          = lookupProfile(odsa, modelIRI, pf, profiles).map { pfR =>
            Vector(Tuple3(
              pf,
              pfR,
              appliedStereotypes))
          }
          acc append inc
        }
    },
    _ append _)

  def lookupProfile
  (odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   applyingModel: common.ResourceIRI,
   pf: UMLProfile[MagicDrawUML],
   profiles: Vector[OTIMOFProfileTables])
  : Vector[java.lang.Throwable] \&/ OTIMOFProfileTables
  = odsa.ds.lookupDocumentByExtent(pf) match {
    case None =>
      \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLProfile[MagicDrawUML]](
        s"No OTI Document for profile ${pf.qualifiedName.get}",
        Iterable(pf))))
    case Some(d) =>
      val pfURI = d.toOTIMOFResourceIRI
      profiles
        .find { r => pfURI == r.resourceType.head.resource } match {
        case None =>
          \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLProfile[MagicDrawUML]](
            s"No OTI MOF Profile resource for ${pf.qualifiedName.get} with Profile::URI=$pfURI",
            Iterable(pf))))
        case Some(r) =>
          \&/.That(r)
      }
  }

  def toAppliedStereotypes
  (resource: common.ResourceIRI,
   pfR: OTIMOFProfileTables,
   stereotypedElements: Map[(UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML]), Set[UMLElement[MagicDrawUML]]])
  : Vector[java.lang.Throwable] \&/ Vector[tables.model.OTIMOFAppliedStereotype]
  = stereotypedElements
    .aggregate[\&/[Vector[java.lang.Throwable], Vector[tables.model.OTIMOFAppliedStereotype]]](\&/.That(Vector()))(
    {
      case (acc, ((s, _), es)) =>
        val sUUID = s.toOTIMOFEntityUUID

        val ps = pfR.stereotypes.find { cs =>
          val ok = sUUID.value == cs.uuid.value
          if (ok)
            true
          else
            false
        }

        val inc
        : Vector[java.lang.Throwable] \&/ Vector[tables.model.OTIMOFAppliedStereotype]
        = ps match {
          case None =>
            \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLStereotype[MagicDrawUML]](
              s"*** Unknown stereotype '${s.qualifiedName.get}' applied to ${es.size} elements",
              Iterable(s)
            )))
          case Some(otiS) =>
            \&/.That(es.map { e =>
              tables.model.OTIMOFAppliedStereotype(
                resource,
                modelElement = e.toOTIMOFEntityUUID,
                appliedStereotype = otiS.uuid)
            }
              .toVector)
        }

        acc append inc
    },
    _ append _)

  def exportAsOTIMOFModelExtent
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    pfExtents: Vector[OTIMOFProfileTables],
    d: Document[MagicDrawUML],
    modelIRI: common.ResourceIRI,
    appliedStereotypes: Vector[tables.model.OTIMOFAppliedStereotype],
    allAppliedStereotypesByProfile: AppliedStereotypesByProfile)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ OTIMOFModelTables
  = {
    implicit val ops = odsa.otiAdapter.umlOps
    val ch = odsa.otiAdapter.otiCharacteristicsProvider

    def elementAttributeSeqOp[T]
    (toValues: (MagicDrawUMLElement) => Vector[java.lang.Throwable] \&/ Vector[T])
    ( acc: Vector[java.lang.Throwable] \&/ Vector[T],
      e: MagicDrawUMLElement )
    : Vector[java.lang.Throwable] \&/ Vector[T]
    = acc
      .append(toValues(e))

      //.append(ModelElementAttributeValueExporter.toModelElementStereotypeAttributeValues(e, ch))

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
    : Iterable[tables.model.OTMOFModelElement]
    = parExtent.map(toModelElement(modelIRI)).to[Iterable]

    val elementIDs
    : Iterable[tables.OTIMOFToolSpecificID]
    = parExtent.map(toModelElementID(modelIRI)).to[Iterable]

    val elementURLs
    : Iterable[tables.OTIMOFToolSpecificURL]
    = parExtent.map(toModelElementURL(modelIRI)).to[Iterable]

    val orderedLinks
    : Iterable[tables.model.OTIMOFModelOrderedLink]
    = parExtent.map(ops.umlMagicDrawUMLElement).flatMap(ModelLinkExporter.toOrderedLinks(modelIRI, odsa, cache)).seq

    val unorderedLinks
    : Iterable[tables.model.OTIMOFModelUnorderedLink]
    = parExtent.map(ops.umlMagicDrawUMLElement).flatMap(ModelLinkExporter.toUnorderedLinks(modelIRI, odsa, cache)).seq

    val orderedAtomicValues
    : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeAtomicValue]
    = parExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeAtomicValue]](\&/.That(Vector()))(
      elementAttributeSeqOp[tables.values.OTIMOFOrderedAttributeAtomicValue](ModelElementAttributeValueExporter.toOrderedAtomicValues(modelIRI, ch)), _ append _)

    val orderedLiteralValues
    : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
    = parExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]](\&/.That(Vector()))(
      elementAttributeSeqOp[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue](ModelElementAttributeValueExporter.toOrderedEnumerationValues(modelIRI, ch)), _ append _)

    val unorderedAtomicValues
    : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeAtomicValue]
    = parExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeAtomicValue]](\&/.That(Vector()))(
      elementAttributeSeqOp[tables.values.OTIMOFUnorderedAttributeAtomicValue](ModelElementAttributeValueExporter.toUnorderedAtomicValues(modelIRI, ch)), _ append _)

    val unorderedLiteralValues
    : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]
    = parExtent
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]](\&/.That(Vector()))(
      elementAttributeSeqOp[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue](ModelElementAttributeValueExporter.toUnorderedEnumerationValues(modelIRI, ch)), _ append _)

    val instantiatedMetamodels
    : Iterable[OTIMOFResourceInstantiatedMetamodel]
    = Iterable(OTIMOFResourceInstantiatedMetamodel(
      instantiatedMetamodel = cache.resolver.umlR.resourceType.head.resource,
      instantiatingModel = modelIRI))

    val appliedProfiles
    : Iterable[OTIMOFResourceModelAppliedProfile]
    = parExtent
      .flatMap {
        case pa: UMLProfileApplication[MagicDrawUML] =>
          (pa.applyingPackage, pa.appliedProfile) match {
            case (Some(pkg), Some(pf)) =>
              for {
                dpf <- odsa.ds.lookupDocumentByExtent(pf)
              } yield OTIMOFResourceModelAppliedProfile(
                applyingResource = modelIRI,
                applyingPackage = pkg.toOTIMOFEntityUUID,
                appliedResource = dpf.toOTIMOFResourceIRI,
                appliedProfile = pf.toOTIMOFEntityUUID)

            case _ =>
              None
          }
        case _ =>
          None
      }
      .to[Iterable]

    val orderedStereotypeReferences
    : Iterable[tables.model.OTIMOFAppliedStereotypePropertyOrderedReference]
    = parExtent.flatMap(ModelStereotypeTagPropertyExporter.toOrderedReferences(modelIRI, odsa, cache)).to[Iterable]

    val unorderedStereotypeReferences
    : Iterable[tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference]
    = parExtent.flatMap(ModelStereotypeTagPropertyExporter.toUnorderedReferences(modelIRI, odsa, cache)).to[Iterable]

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val extent = OTIMOFModelTables(
      resourceType =
        Iterable(tables.OTIMOFResourceType(resource=modelIRI, kind=tables.OTIMOFResourceModelKind)),
      instantiatedMetamodels,
      appliedProfiles,
      elements, elementIDs, elementURLs,
      orderedAtomicValues = orderedAtomicValues.b.getOrElse(Vector.empty),
      orderedLiteralValues = orderedLiteralValues.b.getOrElse(Vector.empty),
      orderedStructuredValues = Iterable.empty[tables.values.OTIMOFOrderedAttributeStructuredValueLink],
      unorderedAtomicValues = unorderedAtomicValues.b.getOrElse(Vector.empty),
      unorderedLiteralValues = unorderedLiteralValues.b.getOrElse(Vector.empty),
      unorderedStructuredValues = Iterable.empty[tables.values.OTIMOFUnorderedAttributeStructuredValueLink],
      appliedStereotypes,
      orderedStereotypeReferences,
      unorderedStereotypeReferences,
      orderedLinks,
      unorderedLinks)

    guiLog.log(s"Model Extent: ${d.info.packageURI}")

    \&/.That(extent)
  }

  def toModelElement
  (resource: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : tables.model.OTMOFModelElement
  = tables.model.OTMOFModelElement(
    resource,
    uuid = e.toOTIMOFEntityUUID,
    metaClass = cache.resolver.mcName2UUID(e.mofMetaclassName))

  def toModelElementID
  (resource: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : tables.OTIMOFToolSpecificID
  = tables.OTIMOFToolSpecificID(
    resource,
    uuid = e.toOTIMOFEntityUUID,
    toolVendorID = "MagicDraw",
    toolElementID = OTIPrimitiveTypes.TOOL_SPECIFIC_ID.unwrap(e.toolSpecific_id))

  def toModelElementURL
  (resource: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : tables.OTIMOFToolSpecificURL
  = tables.OTIMOFToolSpecificURL(
    resource,
    uuid = e.toOTIMOFEntityUUID,
    toolVendorID = "MagicDraw",
    toolElementURL = OTIPrimitiveTypes.TOOL_SPECIFIC_URL.unwrap(e.toolSpecific_url))


}