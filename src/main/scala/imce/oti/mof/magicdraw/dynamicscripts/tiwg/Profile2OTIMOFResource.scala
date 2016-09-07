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

//import java.io.File
import java.nio.file.Path
import java.lang.System

import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

//import play.api.libs.json._

import scala.collection.immutable._
import scala.{Int, None, Some, StringContext, Tuple4}
//import scala.Predef.{augmentString,require,String}
//import scala.util.control.Exception._
import scalaz._

object Profile2OTIMOFResource {

  def profile2OTIMOFResource
  (resultDir: Path,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   resolver: views.UMLMetamodelResolver,
   pf: UMLProfile[MagicDrawUML],
   d: Document[MagicDrawUML])
  : Vector[java.lang.Throwable] \&/ Vector[OTIMOFProfileTables]
  = {
    implicit val ops = odsa.otiAdapter.umlOps

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
        .flatMap { case (p, i) =>
          p._type match {
            case Some(dt: UMLDataType[MagicDrawUML]) =>
              dt.name match {
                case None =>
                  throw new java.lang.IllegalArgumentException(
                    s"Stereotype property ${p.qualifiedName.get} must be typed by a named data type")
                case Some(dtName) =>
                  resolver.primitiveTypesR.classifiers.find(_.name.value == dtName).map { cls =>
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
                  throw new java.lang.IllegalArgumentException(
                    s"Stereotype property ${p.qualifiedName.get} must be typed by a named class")
                case Some(mcName) =>
                  resolver.mcName2UUID.get(mcName) match {
                    case None =>
                      throw new java.lang.IllegalArgumentException(
                        s"Stereotype property ${p.qualifiedName.get} must be typed by a known metaclass!")
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
            case Some(ps: UMLStereotype[MagicDrawUML]) if ss.contains(ps) =>
              Some((s, p, ps))
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    val profileIRI = d.toOTIMOFResourceIRI

    val exported
    : Vector[java.lang.Throwable] \&/ Vector[OTIMOFProfileTables]
    = for {
      s2mc <- ExportAsOTIMOFProfiles.getStereotype2ExtendedMetaclasses(profileIRI, ss, resolver.umlR)
      ext = OTIMOFProfileTables(
        resourceType = Iterable(tables.OTIMOFResourceType(resource=profileIRI, kind=tables.OTIMOFResourceProfileKind)),
        extendedMetamodels = Iterable(tables.profile.OTIMOFProfile2ExtendedMetamodel(
          resource = profileIRI,
          extendedMetamodel = resolver.umlR.resourceType.head.resource)),
        importedProfiles = pf.packageImport.toVector.flatMap { pi =>
          pi.importedPackage match {
            case Some(ipf: UMLProfile[MagicDrawUML]) =>
              odsa.ds.lookupDocumentByScope(ipf) match {
                case Some(dipf) =>
                  Some(OTIMOFResourceProfileImport(
                    importingProfile = profileIRI,
                    importedProfile = common.ResourceIRI(OTIPrimitiveTypes.OTI_URI.unwrap(dipf.info.packageURI))))
                case _ =>
                  System.out.println(
                    s"Profile `${pf.qualifiedName.get}` imports " +
                      s"a profile without a known OTI Document PackageURI: `${ipf.qualifiedName.get}`")
                  None
              }
            case _ =>
              None
          }
        },
        importedLibraries = Iterable.empty[OTIMOFResourceLibraryImport], // @TODO
        stereotypes = ss.map { s =>
          tables.profile.OTIMOFStereotype(
            resource = profileIRI,
            uuid = s.toOTIMOFEntityUUID,
            name = common.Name(s.name.get),
            isAbstract = s.isAbstract)
        },

        generalizations = ss.flatMap { s =>
          s.general_classifier.flatMap {
            case sp: UMLStereotype[MagicDrawUML] =>
              Some(tables.profile.OTIMOFStereotypeGeneralization(
                resource = profileIRI,
                general = s.toOTIMOFEntityUUID,
                specific = sp.toOTIMOFEntityUUID))
            case _ =>
              None
          }
        },
        extendedMetaclasses = s2mc,
        stereotypeAttributes = sas.map { case (s, f, _, i) =>
          tables.profile.OTIMOFStereotype2Attribute(
            resource = profileIRI,
            stereotype = s.toOTIMOFEntityUUID,
            attribute = f.toOTIMOFEntityUUID,
            index = i)
        },
        stereotype2MetaClassProperty = sms.map { case (s, f, mcUUID) =>
          tables.profile.OTIMOFStereotypeAssociationTargetEndMetaClassProperty(
            resource = profileIRI,
            sourceStereotype = s.toOTIMOFEntityUUID,
            associationTargetEnd = f.toOTIMOFEntityUUID,
            targetMetaClass = mcUUID)
        },
        stereotype2StereotypeProperty = sss.map { case (s, f, st) =>
          tables.profile.OTIMOFStereotypeAssociationTargetEndStereotypeProperty(
            resource = profileIRI,
            sourceStereotype = s.toOTIMOFEntityUUID,
            associationTargetEnd = f.toOTIMOFEntityUUID,
            targetStereotype = st.toOTIMOFEntityUUID)
        },
        associationTargetEnds = sms.map { case (_, p, _) =>
          if (p.isComposite)
            features.AssociationTargetCompositeEnd(
              resource = profileIRI,
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
          else
            features.AssociationTargetReferenceEnd(
              resource = profileIRI,
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
        } ++ sss.map { case (_, p, _) =>
          if (p.isComposite)
            features.AssociationTargetCompositeEnd(
              resource = profileIRI,
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
          else
            features.AssociationTargetReferenceEnd(
              resource = profileIRI,
              uuid = p.toOTIMOFEntityUUID,
              name = common.Name(p.name.get))
        },
        attributes = sas.map { case (_, f, _, _) =>
          features.DataTypedAttributeProperty(
            resource = profileIRI,
            uuid = f.toOTIMOFEntityUUID,
            name = common.Name(f.name.get))
        },
        featureLowerBounds = sas.map { case (_, f, _, _) =>
          features.FeatureLowerBound(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            lower = common.NonNegativeInt(f.lower.intValue()))
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureLowerBound(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            lower = common.NonNegativeInt(f.lower.intValue()))
        },
        featureUpperBounds = sas.map { case (_, f, _, _) =>
          features.FeatureUpperBound(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            upper = common.UnlimitedNatural(f.upper.intValue()))
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureUpperBound(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            upper = common.UnlimitedNatural(f.upper.intValue()))
        },
        featureOrdering = sas.map { case (_, f, _, _) =>
          features.FeatureOrdering(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            isOrdered = f.isOrdered)
        } ++ (sms ++ sss).map { case (_, f, _) =>
          features.FeatureOrdering(
            resource = profileIRI,
            feature = f.toOTIMOFEntityUUID,
            isOrdered = f.isOrdered)
        },
        attribute2type = sas.map { case (_, f, dtUUID, _) =>
          features.AttributeProperty2DataType(
            resource = profileIRI,
            attribute = f.toOTIMOFEntityUUID,
            `type` = dtUUID)
        })
    } yield {
      System.out.println(s"Extent: ${d.info.packageURI}")
      Vector(ext)
    }

    exported
  }

}