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

import imce.oti.mof.resolvers.UMLMetamodelResolver

import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.ui.ProfileInspectorWidget

import org.omg.oti.json.common.{OTIDocumentSetConfiguration,OTIPrimitiveTypes}
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Int, None, Option, PartialFunction, Some, StringContext, Tuple4}
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

  import Utils.VectorSemigroup

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
    exportAsOTIMOFProfileCallback,
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
    exportAsOTIMOFProfileCallback,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel)

  def exportAsOTIMOFProfileCallback
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent],
    config: OTIDocumentSetConfiguration,
    selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]] )
  : Try[Option[MagicDrawValidationDataResults]]
  = for {
    cb <- exportAsOTIMOFProfile(p, odsa, resourceExtents)
    er <- Utils.exportAsOTIMOFResource(
      p, odsa, config,
      selectedSpecificationRootPackages,
      resourceExtents, cb, "exportAsOTIMOFLibrary")
  } yield er

  def exportAsOTIMOFProfile
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[(Document[MagicDrawUML], Set[Document[MagicDrawUML]])=> \&/[Vector[java.lang.Throwable], OTIMOFProfileResourceExtent]]
  = resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
    case Some(primitiveTypesR: OTIMOFLibraryResourceExtent) =>
      resourceExtents.find(Utils.UML25_IRI == _.resource.iri) match {
        case Some(umlR: OTIMOFMetamodelResourceExtent) =>
          val umlResolver = UMLMetamodelResolver.initialize(primitiveTypesR, umlR)
          Success(exportAsOTIMOFProfile(p, odsa, umlResolver) _)
        case _ =>
          Failure(new java.lang.IllegalArgumentException("No MD18 UML2.5 metamodel resource found!"))
      }
    case _ =>
      Failure(new java.lang.IllegalArgumentException("No MD18 PrimitiveTypes library resource found!"))
  }

  def exportAsOTIMOFProfile
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    umlResolver: UMLMetamodelResolver )
  ( d: Document[MagicDrawUML],
    pfDocuments: Set[Document[MagicDrawUML]] )
  : Vector[java.lang.Throwable] \&/ OTIMOFProfileResourceExtent
  = d.scope match {
    case pf: UMLProfile[MagicDrawUML] =>
      exportAsOTIMOFProfile(p, odsa, d, pf, pfDocuments)(umlResolver)
    case pkg =>
      \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
        s"Cannot export package ${pkg.qualifiedName.get} as an OTIMOFProfile",
        Iterable(pkg))))
  }

  def exportAsOTIMOFProfile
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    d: Document[MagicDrawUML],
    pf: UMLProfile[MagicDrawUML],
    pfDocuments: Set[Document[MagicDrawUML]] )
  (implicit umlResolver: UMLMetamodelResolver)
  : Vector[java.lang.Throwable] \&/ OTIMOFProfileResourceExtent
  = {
    val app = Application.getInstance()
    val guiLog = app.getGUILog

    implicit val ops = odsa.otiAdapter.umlOps
    import umlResolver._

    val allImportedPFs = pf.allImportedProfilesTransitively

    val ss
    : Vector[UMLStereotype[MagicDrawUML]]
    = pf
      .allApplicableStereotypes
      .to[Vector]
      .sortBy(_.name.get)

    val sas
    : Vector[(UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML], common.EntityUUID, Int)]
    = ss.flatMap { s =>
      s
        .ownedAttribute
        .zipWithIndex
        .flatMap { case (p,i) =>
        p._type match {
          case Some(dt: UMLDataType[MagicDrawUML]) =>
            dt.name match {
              case None =>
                throw new java.lang.IllegalArgumentException(s"Stereotype property ${p.qualifiedName.get} must be typed by a named data type")
              case Some(dtName) =>
                primitiveTypesR.classifiers.find(_.name.value == dtName).map { cls =>
                  Tuple4(s, p, cls.uuid, i)
                }
            }
          case _ =>
            None
        }
      }
    }

    val sms
    : Vector[(UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML], common.EntityUUID)]
    = ss.flatMap { s =>
      s.attribute.flatMap {
        case p if !p.isDerived && p.extension.isEmpty =>
          p._type match {
            case Some(_: UMLStereotype[MagicDrawUML]) =>
              None
            case Some(mc: UMLClass[MagicDrawUML]) =>
              mc.name match {
                case None =>
                  throw new java.lang.IllegalArgumentException(s"Stereotype property ${p.qualifiedName.get} must be typed by a named class")
                case Some(mcName) =>
                  mcName2UUID.get(mcName) match {
                    case None =>
                      throw new java.lang.IllegalArgumentException(s"Stereotype property ${p.qualifiedName.get} must be typed by a known metaclass!")
                    case Some(mcUUID) =>
                      Some((s, p, mcUUID))
                  }
              }
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    val sss
    : Vector[(UMLStereotype[MagicDrawUML], UMLProperty[MagicDrawUML], UMLStereotype[MagicDrawUML])]
    = ss.flatMap { s =>
      s.attribute.flatMap {
        case p if !p.isDerived =>
          p._type match {
            // @todo check for ps defined in an exported profile.
            case Some(ps: UMLStereotype[MagicDrawUML]) /* if ss.contains(ps) */ =>
              Some((s, p, ps))
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    for {
      s2mc <- getStereotype2ExtendedMetaclasses(ss, umlR)
      ext = OTIMOFProfileResourceExtent(
        resource = OTIMOFProfile(d.toOTIMOFResourceIRI),
        extendedMetamodels = Vector(profile.Profile2ExtendedMetamodel(
          extendedMetamodel = umlR.resource.iri,
          extendingProfile = d.toOTIMOFResourceIRI)),
        importedProfiles = pf.packageImport.toVector.flatMap { pi =>
          pi.importedPackage match {
            case Some(ipf: UMLProfile[MagicDrawUML]) =>
              pfDocuments.find(_.scope == ipf) match {
                case Some(dipf) =>
                  Some(OTIMOFResourceProfileImport(
                    importingProfile = d.toOTIMOFResourceIRI,
                    importedProfile = common.ResourceIRI(OTIPrimitiveTypes.OTI_URI.unwrap(dipf.info.packageURI))))
                case _ =>
                  java.lang.System.out.println(
                    s"Profile `${pf.qualifiedName.get}` imports "+
                    s"a profile without a known OTI Document PackageURI: `${ipf.qualifiedName.get}`")
                  None
              }
            case _ =>
              None
          }
        },
        classifiers = ss.map { s =>
          profile.Stereotype(
            uuid = s.toOTIMOFEntityUUID,
            name = common.Name(s.name.get))
        },
        associationTargetEnds = sms.map { case (_, p, _) =>
          if (p.isComposite)
            features.AssociationTargetCompositeEnd(
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
          else
            features.AssociationTargetReferenceEnd(
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
        } ++ sss.map { case (_, p, _) =>
          if (p.isComposite)
            features.AssociationTargetCompositeEnd(
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
          else
            features.AssociationTargetReferenceEnd(
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
        },
        generalizations = ss.flatMap { s =>
          s.general_classifier.flatMap {
            case sp: UMLStereotype[MagicDrawUML] =>
              Some(profile.StereotypeGeneralization(
                general = s.toOTIMOFEntityUUID,
                specific = sp.toOTIMOFEntityUUID))
            case _ =>
              None
          }
        },
        attributes = sas.map { case (_, f, _, _) =>
            features.DataTypedAttributeProperty(
                uuid = f.toOTIMOFEntityUUID,
                name = common.Name(f.name.get))
        },
        featureLowerBounds = sas.map { case (_, f, _, _) =>
          features.FeatureLowerBound(
              feature = f.toOTIMOFEntityUUID,
              lower = common.NonNegativeInt(f.lower.intValue()))
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureLowerBound(
            feature = f.toOTIMOFEntityUUID,
            lower = common.NonNegativeInt(f.lower.intValue()))
        },
        featureUpperBounds = sas.map { case (_, f, _, _) =>
          features.FeatureUpperBound(
            feature = f.toOTIMOFEntityUUID,
            upper = common.UnlimitedNatural(f.upper.intValue()))
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureUpperBound(
            feature = f.toOTIMOFEntityUUID,
            upper = common.UnlimitedNatural(f.upper.intValue()))
        },
        featureOrdering = sas.map { case (_, f, _, _) =>
          features.FeatureOrdering(
            feature = f.toOTIMOFEntityUUID,
            isOrdered = f.isOrdered)
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureOrdering(
            feature = f.toOTIMOFEntityUUID,
            isOrdered = f.isOrdered)
        },
        extendedMetaclass = s2mc,
        stereotype2attribute = sas.map { case (s, f, _, i) =>
            profile.Stereotype2Attribute(
              stereotype = s.toOTIMOFEntityUUID,
              attribute = f.toOTIMOFEntityUUID,
              index = i)
        },
        attribute2type = sas.map { case (_, f, dtUUID, _) =>
          features.AttributeProperty2DataType(
            attribute = f.toOTIMOFEntityUUID,
            `type` = dtUUID)
        },
        stereotype2associationEndMetaClassProperty = sms.map { case (s, f, mcUUID) =>
            profile.StereotypeAssociationTargetEndMetaClassProperty(
              sourceStereotype = s.toOTIMOFEntityUUID,
              associationTargetEnd = f.toOTIMOFEntityUUID,
              targetMetaClass = mcUUID)
        },
        stereotype2associationEndStereotypeProperty = sss.map { case (s, f, st) =>
          profile.StereotypeAssociationTargetEndStereotypeProperty(
            sourceStereotype = s.toOTIMOFEntityUUID,
            associationTargetEnd = f.toOTIMOFEntityUUID,
            targetStereotype = st.toOTIMOFEntityUUID)
        })
    } yield {
      guiLog.log(s"Profile Extent: ${d.info.packageURI}")
      ext
    }
  }

  def getStereotype2ExtendedMetaclasses
  ( ss: Vector[UMLStereotype[MagicDrawUML]],
    umlR: OTIMOFMetamodelResourceExtent )
  : Vector[java.lang.Throwable] \&/ Vector[profile.Stereotype2ExtendedMetaclass]
  = ss.aggregate[\&/[Vector[java.lang.Throwable], Vector[profile.Stereotype2ExtendedMetaclass]]](\&/.That(Vector()))(
    {
      case (acc1, s) =>
        s.baseMetaProperties.aggregate(acc1)(
          {
            case (acc2, baseP) =>
              import Utils.VectorSemigroup
              acc2 append getStereotypeBaseProperty2ExtendedMetaclass(s, baseP, umlR)
          },
          _ append _)
    },
    _ append _)

  def getStereotypeBaseProperty2ExtendedMetaclass
  ( s: UMLStereotype[MagicDrawUML],
    baseP: UMLProperty[MagicDrawUML],
    umlR: OTIMOFMetamodelResourceExtent )
  : Vector[java.lang.Throwable] \&/ Vector[profile.Stereotype2ExtendedMetaclass]
  = baseP._type match {
    case None =>
      \&/.This(Vector(
        UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
          s"Stereotype base property ${baseP.qualifiedName.get} should be typed by a metaclass",
          Iterable(baseP))))
    case Some(extMC) =>
      import Utils.selectable

      umlR
        .classifiers
        .select { case umlMC: metamodel.MetaClass => umlMC }
        .find { umlMC => umlMC.name.value == extMC.name.get } match {
        case None =>
          \&/.This(Vector(
            UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
              s"Stereotype base property ${baseP.qualifiedName.get} refers to an unknown metaclass ${extMC.qualifiedName.get}",
              Iterable(baseP))))
        case Some(umlMC) =>
          \&/.That(Vector(
            profile.Stereotype2ExtendedMetaclass(
              extendingStereotype = s.toOTIMOFEntityUUID,
              extendedMetaclass = umlMC.uuid)))
      }
  }

}