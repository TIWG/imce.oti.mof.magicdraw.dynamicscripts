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
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults

import imce.oti.mof.resolvers.OTIHelpers._

import org.omg.oti.json.common.OTIDocumentSetConfiguration
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Int, None, Option, Some, StringContext}
import scala.Predef.require
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
    exportAsOTIMOFMetamodelCallback,
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
    exportAsOTIMOFMetamodelCallback,
    Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypes)

  def exportAsOTIMOFMetamodelCallback
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent],
    config: OTIDocumentSetConfiguration,
    selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]] )
  : Try[Option[MagicDrawValidationDataResults]]
  = for {
    cb <- exportAsOTIMOFMetamodel(p, odsa, resourceExtents)
    er <- Utils.exportAsOTIMOFResource(
      p, odsa, config,
      selectedSpecificationRootPackages,
      resourceExtents, cb, "exportAsOTIMOFLibrary")
  } yield er

  def exportAsOTIMOFMetamodel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    resourceExtents: Set[OTIMOFResourceExtent])
  : Try[(Document[MagicDrawUML], Set[Document[MagicDrawUML]])=> \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent]]
  = resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
    case Some(r: OTIMOFLibraryResourceExtent) =>
      Success(exportAsOTIMOFMetamodel(p, odsa, r) _)
    case _ =>
      Failure(new java.lang.IllegalArgumentException("No MD18 PrimitiveTypes library resource found!"))
  }

  case class UMLAssociationInfo
  ( a: UMLAssociation[MagicDrawUML],
    source: UMLProperty[MagicDrawUML],
    target: UMLProperty[MagicDrawUML] )

  def exportAsOTIMOFMetamodel
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    primitiveTypes: OTIMOFLibraryResourceExtent)
  ( d: Document[MagicDrawUML],
    pfDocuments: Set[Document[MagicDrawUML]] )
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
    : Vector[UMLAssociationInfo]
    = d
      .scope
      .allOwnedElements
      .select { case mc: UMLAssociation[MagicDrawUML] => mc }
      .to[Vector]
      .sortBy(_.name.get)
      .map { ma =>
        val ends = ma.getDirectedAssociationEnd
        require(ends.isDefined, ma.qualifiedName.get)
        val (target, source) = ends.get
        val targetName = target.name.get
        if (targetName.startsWith("_") || targetName.startsWith("UML")) {
          System.out.println(s"Assoc (rev) ${ma.name.get}\n ${source._type.get.name.get}::${target.name.get} -> ${source.name.get}::${target._type.get.name.get}")
          UMLAssociationInfo(ma, target, source)
        } else {
          System.out.println(s"Assoc (fwd) ${ma.name.get}\n ${target._type.get.name.get}::${source.name.get} -> ${target.name.get}::${source._type.get.name.get}")
          UMLAssociationInfo(ma, source, target)
        }
      }

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val atomicAttributes
    : Vector[(common.EntityUUID, UMLProperty[MagicDrawUML], Int)]
    = mcs
      .flatMap { mc =>
        val mcUUID = mc.toOTIMOFEntityUUID
        mc
          .ownedAttribute
          .zipWithIndex
          .filter(_._1._type match {
            case Some(_: UMLPrimitiveType[MagicDrawUML]) =>
              true
            case _ =>
              false
          })
          .map { case (p, i) =>
            (mcUUID, p, i)
          }
      }

    val enumerationAttributes
    : Vector[(common.EntityUUID, UMLProperty[MagicDrawUML], Int)]
    = mcs
      .flatMap { mc =>
        val mcUUID = mc.toOTIMOFEntityUUID
        mc
          .ownedAttribute
          .zipWithIndex
          .filter(_._1._type match {
            case Some(_: UMLEnumeration[MagicDrawUML]) =>
              true
            case _ =>
              false
          })
          .map { case (p, i) =>
            (mcUUID, p, i)
          }
      }

    val structuredAttributes
    : Vector[(common.EntityUUID, UMLProperty[MagicDrawUML], Int)]
    = mcs
      .flatMap { mc =>
        val mcUUID = mc.toOTIMOFEntityUUID
        mc
          .ownedAttribute
          .zipWithIndex
          .filter(_._1._type match {
            case Some(_: UMLPrimitiveType[MagicDrawUML]) =>
              false
            case Some(_: UMLEnumeration[MagicDrawUML]) =>
              false
            case Some(_: UMLDataType[MagicDrawUML]) =>
              true
            case _ =>
              false
          })
          .map { case (p, i) =>
            (mcUUID, p, i)
          }
      }

    val allAttributes = atomicAttributes ++ enumerationAttributes ++ structuredAttributes

    val metamodelIRI = d.toOTIMOFResourceIRI
    val extent = OTIMOFMetamodelResourceExtent(
      resource = OTIMOFMetamodel(metamodelIRI),
      importedLibraries = Vector(
        OTIMOFResourceLibraryImport(
          importingResource = d.toOTIMOFResourceIRI,
          importedLibrary = primitiveTypes.resource.iri)),
      classifiers =
        mcs.map { mc =>
          metamodel.MetaClass(
            uuid = mc.toOTIMOFEntityUUID,
            name = common.Name(mc.name.get))
        } ++
          mas.map { case UMLAssociationInfo(ma, source, target) =>
            metamodel.MetaAssociation(
              uuid = ma.toOTIMOFEntityUUID,
              name = common.Name(ma.name.get))
          },
      associationEnds = mas.flatMap { case UMLAssociationInfo(ma, source, target) =>

        Vector(
          features.AssociationSourceEnd(
            resource = metamodelIRI,
            uuid = source.toOTIMOFEntityUUID,
            name = common.Name(source.name.get)),
          if (target.isComposite)
            features.AssociationTargetCompositeEnd(
              resource = metamodelIRI,
              uuid = target.toOTIMOFEntityUUID,
              name = common.Name(target.name.get))
          else
            features.AssociationTargetReferenceEnd(
              resource = metamodelIRI,
              uuid = target.toOTIMOFEntityUUID,
              name = common.Name(target.name.get))
        )
      },
      association2source = mas.map { case UMLAssociationInfo(ma, source, target) =>
        metamodel.MetaAssociation2SourceEndProperty(
          association = ma.toOTIMOFEntityUUID,
          sourceEnd = source.toOTIMOFEntityUUID
        )
      },
      association2target = mas.map { case UMLAssociationInfo(ma, source, target) =>
        metamodel.MetaAssociation2TargetEndProperty(
          association = ma.toOTIMOFEntityUUID,
          targetEnd = target.toOTIMOFEntityUUID
        )
      },
      associationEnd2Metaclass = mas.flatMap { case UMLAssociationInfo(ma, source, target) =>
        Vector(
          metamodel.MetaAssociationEndProperty2MetaClassType(
            associationEnd = source.toOTIMOFEntityUUID,
            `type` = source.getMetaClassUUID()),
          metamodel.MetaAssociationEndProperty2MetaClassType(
            associationEnd = target.toOTIMOFEntityUUID,
            `type` = target.getMetaClassUUID())
        )
      },
      attributes = allAttributes.map { case (mcUUID, p, _) =>
        features.DataTypedAttributeProperty(
          resource = metamodelIRI,
          uuid = p.toOTIMOFEntityUUID,
          name = common.Name(p.name.get))
      },
      attribute2type = allAttributes.map { case (_, p, _) =>
          features.AttributeProperty2DataType(
            resource = metamodelIRI,
            attribute = p.toOTIMOFEntityUUID,
            `type` = p.getSchemaDatatypeUUID()
          )
      },
      metaclass2orderedAtomicAttribute = atomicAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaclass2orderedEnumerationAttribute = enumerationAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaclass2orderedStructuredAttribute = structuredAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaclass2unorderedAtomicAttribute = atomicAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaclass2unorderedEnumerationAttribute = enumerationAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaclass2unorderedStructuredAttribute = structuredAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(metamodel.MetaClass2Attribute(
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      featureLowerBounds = mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureLowerBound(
          resource = metamodelIRI,
          feature = source.toOTIMOFEntityUUID,
          lower = common.NonNegativeInt(source.lower.intValue())
        )
      } ++ mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureLowerBound(
          resource = metamodelIRI,
          feature = target.toOTIMOFEntityUUID,
          lower = common.NonNegativeInt(target.lower.intValue())
        )
      } ++ allAttributes.map { case (_, p, _) =>
        features.FeatureLowerBound(
          resource = metamodelIRI,
          feature = p.toOTIMOFEntityUUID,
          lower = common.NonNegativeInt(p.lower.intValue()))
      },
      featureUpperBounds = mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureUpperBound(
          resource = metamodelIRI,
          feature = source.toOTIMOFEntityUUID,
          upper = common.UnlimitedNatural(source.upper.intValue()))
      } ++ mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureUpperBound(
          resource = metamodelIRI,
          feature = target.toOTIMOFEntityUUID,
          upper = common.UnlimitedNatural(target.upper.intValue()))
      } ++ allAttributes.map { case (_, p, _) =>
        features.FeatureUpperBound(
          resource = metamodelIRI,
          feature = p.toOTIMOFEntityUUID,
          upper = common.UnlimitedNatural(p.upper.intValue()))
      },
      featureOrdering = mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureOrdering(
          resource = metamodelIRI,
          feature = source.toOTIMOFEntityUUID,
          isOrdered = source.isOrdered)
      } ++ mas.map { case UMLAssociationInfo(ma, source, target) =>
        features.FeatureOrdering(
          resource = metamodelIRI,
          feature = target.toOTIMOFEntityUUID,
          isOrdered = target.isOrdered)
      } ++ allAttributes.map { case (_, p, _) =>
        features.FeatureOrdering(
          resource = metamodelIRI,
          feature = p.toOTIMOFEntityUUID,
          isOrdered = p.isOrdered)
      },
      generalizations = mcs.flatMap { mc =>
        val specific = mc.toOTIMOFEntityUUID
        mc.parents.flatMap {
          case pc: UMLClass[MagicDrawUML] if mcs.contains(pc) =>
            Some(metamodel.MetaClassifierGeneralization(
              specific,
              general=pc.toOTIMOFEntityUUID))
          case _ =>
            None
        }
      } ++ mas.flatMap { case UMLAssociationInfo(ma, source, target) =>
        val specific = ma.toOTIMOFEntityUUID
        ma.parents.flatMap {
          case pa: UMLAssociation[MagicDrawUML] =>
            Some(metamodel.MetaClassifierGeneralization(
              specific,
              general=pa.toOTIMOFEntityUUID))
          case _ =>
            None
        }
      }
    )

    guiLog.log(s"Metamodel Extent: ${d.info.packageURI}")

    \&/.That(extent)
  }
}