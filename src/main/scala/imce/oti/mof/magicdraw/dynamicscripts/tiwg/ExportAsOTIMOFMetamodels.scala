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

import java.lang.System

import com.nomagic.magicdraw.core.{Application, Project}
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Int, None, Some, StringContext}
import scala.Predef.require
import scalaz._

/**
  * Export selected UML Packages as OTI MOF Metamodels, that is, the exported OTIMOFMetamodelResourceExtents
  * will only have the UML Classes & Associations corresponding to an OTI MOF metamodel, which is a restricted
  * kind of OMG MOF 2.5.1 CMOF metamodel.
  *
  * Everything outside the scope of an OTIMOFMetamodelResourceExtent is ignored
  */
object ExportAsOTIMOFMetamodels {

  case class UMLAssociationInfo
  ( a: UMLAssociation[MagicDrawUML],
    source: UMLProperty[MagicDrawUML],
    target: UMLProperty[MagicDrawUML] )

  def exportAsOTIMOFMetamodelTables
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    d: Document[MagicDrawUML],
    pkg: UMLPackage[MagicDrawUML])
  : Vector[java.lang.Throwable] \&/ Vector[OTIMOFMetamodelTables]
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
          .filter(!_.isDerived)
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
    val im = pkg.packageImport.toVector.flatMap { pi =>
      pi.importedPackage match {
        case Some(ipkg: UMLPackage[MagicDrawUML]) =>
          odsa.ds.lookupDocumentByScope(ipkg) match {
            case Some(dipkg) =>
              Some(OTIMOFResourceMetamodelImport(
                importingMetamodel = metamodelIRI,
                importedMetamodel = dipkg.toOTIMOFResourceIRI))
            case _ =>
              System.out.println(
                s"Profile `${pkg.qualifiedName.get}` imports " +
                  s"a package without a known OTI Document PackageURI: `${ipkg.qualifiedName.get}`")
              None
          }
        case _ =>
          None
      }
    }

    val il = pkg.packageImport.toVector.flatMap { pi =>
      pi.importedPackage match {
        case Some(ipkg: UMLPackage[MagicDrawUML]) =>
          odsa.ds.lookupDocumentByScope(ipkg) match {
            case Some(dipkg) =>
              Some(OTIMOFResourceLibraryImport(
                importingResource = metamodelIRI,
                importedLibrary = dipkg.toOTIMOFResourceIRI))
            case _ =>
              System.out.println(
                s"Profile `${pkg.qualifiedName.get}` imports " +
                  s"a package without a known OTI Document PackageURI: `${ipkg.qualifiedName.get}`")
              None
          }
        case _ =>
          None
      }
    }

    val gs = mcs.flatMap { mc =>
      val specific = mc.toOTIMOFEntityUUID
      mc.parents.flatMap {
        case pc: UMLClass[MagicDrawUML] if mcs.contains(pc) =>
          Some(tables.metamodel.OTIMOFMetaCLassifierGeneralization(
            resource = metamodelIRI,
            specific,
            general=pc.toOTIMOFEntityUUID))
        case _ =>
          None
      }
    } ++ mas.flatMap { case UMLAssociationInfo(ma, source, target) =>
      val specific = ma.toOTIMOFEntityUUID
      ma.parents.flatMap {
        case pa: UMLAssociation[MagicDrawUML] =>
          Some(tables.metamodel.OTIMOFMetaCLassifierGeneralization(
            resource = metamodelIRI,
            specific,
            general=pa.toOTIMOFEntityUUID))
        case _ =>
          None
      }
    }

    val ma2s = mas.map { case UMLAssociationInfo(ma, source, _) =>
      tables.metamodel.OTIMOFMetaAssociation2SourceEndProperty(
        resource = metamodelIRI,
        association = ma.toOTIMOFEntityUUID,
        sourceEnd = source.toOTIMOFEntityUUID)
    }

    val ma2t = mas.map { case UMLAssociationInfo(ma, _, target) =>
      tables.metamodel.OTIMOFMetaAsslcoation2TargetEndProperty(
        resource = metamodelIRI,
        association = ma.toOTIMOFEntityUUID,
        targetEnd = target.toOTIMOFEntityUUID)
    }

    val maes = mas.map { case UMLAssociationInfo(ma, source, _) =>
      features.AssociationSourceEnd(
        resource = metamodelIRI,
        uuid = source.toOTIMOFEntityUUID,
        name = common.Name(source.name.getOrElse("")))
    } ++ mas.map { case UMLAssociationInfo(ma, _, target) =>
      if (target.isComposite)
        features.AssociationTargetCompositeEnd(
          resource = metamodelIRI,
          uuid = target.toOTIMOFEntityUUID,
          name = common.Name(target.name.getOrElse("")))
      else
        features.AssociationTargetReferenceEnd(
          resource = metamodelIRI,
          uuid = target.toOTIMOFEntityUUID,
          name = common.Name(target.name.getOrElse("")))
    }

    val mae2mc =
      mas.map { case UMLAssociationInfo(ma, source, _) =>
        tables.metamodel.OTIMOFMetaAssociationEndProperty2MetaClassType(
          resource = metamodelIRI,
          associationEnd = source.toOTIMOFEntityUUID,
          `type` = source._type.get.toOTIMOFEntityUUID)
      } ++ mas.map { case UMLAssociationInfo(ma, _, target) =>
        tables.metamodel.OTIMOFMetaAssociationEndProperty2MetaClassType(
          resource = metamodelIRI,
          associationEnd = target.toOTIMOFEntityUUID,
          `type` = target._type.get.toOTIMOFEntityUUID)
      }

    val as = allAttributes.map { case (mcUUID, p, _) =>
      features.DataTypedAttributeProperty(
        resource = metamodelIRI,
        uuid = p.toOTIMOFEntityUUID,
        name = common.Name(p.name.get))
    }

    val flb = mas.map { case UMLAssociationInfo(ma, source, target) =>
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
    }

    val fub = mas.map { case UMLAssociationInfo(ma, source, target) =>
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
    }

    val fo = mas.map { case UMLAssociationInfo(ma, source, target) =>
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
    }

    val a2t = allAttributes.map { case (_, p, _) =>
      features.AttributeProperty2DataType(
        resource = metamodelIRI,
        attribute = p.toOTIMOFEntityUUID,
        `type` = p._type.get.toOTIMOFEntityUUID)
    }

    val extent = OTIMOFMetamodelTables(
      resourceType = Iterable(tables.OTIMOFResourceType(resource=metamodelIRI, kind=tables.OTIMOFResourceMetamodelKind)),
      importedMetamodels = im,
      importedLibraries = il,
      
      metaClasses = mcs.map { mc =>
        tables.metamodel.OTIMOFMetaClass(
          resource = metamodelIRI,
          uuid = mc.toOTIMOFEntityUUID,
          name = common.Name(mc.name.get))
      },
      metaAssociations = mas.map { case UMLAssociationInfo(ma, _, _) =>
        tables.metamodel.OTIMOFMetaAssociation(
          resource = metamodelIRI,
          uuid = ma.toOTIMOFEntityUUID,
          name = common.Name(ma.name.get))
      },

      generalizations = gs,
      metaClass2orderedAtomicAttribute = atomicAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaClass2orderedEnumerationAttribute = enumerationAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaClass2orderedStructuredAttribute = structuredAttributes.flatMap {
        case (mcUUID, p, i) if p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaClass2unorderedAtomicAttribute = atomicAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaClass2unorderedEnumerationAttribute = enumerationAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },
      metaClass2unorderedStructuredAttribute = structuredAttributes.flatMap {
        case (mcUUID, p, i) if !p.isOrdered =>
          Some(tables.metamodel.OTIMOFMetaClass2Attribute(
            resource = metamodelIRI,
            metaClass = mcUUID,
            attribute = p.toOTIMOFEntityUUID,
            index = i))
        case _ =>
          None
      },

      metaAssociation2Source = ma2s,
      metaAssociation2Target = ma2t,
      metaAssociationEnds = maes,
      metaAssociationEnd2MetaClass = mae2mc,

      attributes = as,
      featureLowerBounds = flb,
      featureUpperBounds = fub,
      featureOrdering = fo,
      attribute2type = a2t)

    guiLog.log(s"Metamodel Extent: ${d.info.packageURI}")

    \&/.That(Vector(extent))
  }
}