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

import java.io.File
import java.nio.file.Path
import java.lang.System

import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document

import play.api.libs.json._

import scala.collection.immutable._
import scala.{Int, None, Some, StringContext, Tuple2, Tuple4, Unit}
import scala.Predef.{augmentString,require,String}
import scala.util.control.Exception._
import scalaz._

object Profile2OTIMOFResource {

  def profile2OTIMOFResource
  (resultDir: Path,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   resolver: UMLMetamodelResolver,
   pf: UMLProfile[MagicDrawUML],
   d: Document[MagicDrawUML])
  : Vector[java.lang.Throwable] \&/ (Document[MagicDrawUML], OTIMOFProfileResourceExtent)
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

    val exported
    : Vector[java.lang.Throwable] \&/ OTIMOFProfileResourceExtent
    = for {
      s2mc <- ExportAsOTIMOFProfiles.getStereotype2ExtendedMetaclasses(ss, resolver.umlR)
      ext = OTIMOFProfileResourceExtent(
        resource = OTIMOFProfile(d.toOTIMOFResourceIRI),
        extendedMetamodels = Vector(profile.Profile2ExtendedMetamodel(
          extendedMetamodel = resolver.umlR.resource.iri,
          extendingProfile = d.toOTIMOFResourceIRI)),
        importedProfiles = pf.packageImport.toVector.flatMap { pi =>
          pi.importedPackage match {
            case Some(ipf: UMLProfile[MagicDrawUML]) =>
              odsa.ds.lookupDocumentByScope(ipf) match {
                case Some(dipf) =>
                  Some(OTIMOFResourceProfileImport(
                    importingProfile = d.toOTIMOFResourceIRI,
                    importedProfile = common.ResourceIRI(OTIPrimitiveTypes.OTI_URI.unwrap(dipf.info.packageURI))))
                case _ =>
                  java.lang.System.out.println(
                    s"Profile `${pf.qualifiedName.get}` imports " +
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
      System.out.println(s"Extent: ${d.info.packageURI}")
      ext
    }

    import Utils.VectorSemigroup

    exported.flatMap { pfResourceExtent: OTIMOFProfileResourceExtent =>

      catching(nonFatalCatcher)
        .either({

          val uriSuffix: String = OTIPrimitiveTypes.OTI_URI.unwrap(d.info.packageURI).stripPrefix("http://")
          val uriRelpath = (if (uriSuffix.endsWith("/")) uriSuffix else uriSuffix + "/").replace('/', File.separatorChar)
          val pfDir = resultDir.resolve(uriRelpath)
          require(pfDir.toFile.mkdirs(),
            s"Failed to create the directory for profile: ${pf.qualifiedName.get} with URI=${d.info.packageURI}" +
              s" (directory=$pfDir)")

          val pfFile = pfDir.resolve("profile.json").toFile

          val fos = new java.io.FileOutputStream(pfFile)
          val pw = new java.io.PrintWriter(fos)

          try {
            val pfResourceJson = Json.toJson(pfResourceExtent)
            pw.println(Json.prettyPrint(pfResourceJson))

          } finally {
            pw.close()
            fos.close()
          }

        })
        .fold[Vector[java.lang.Throwable] \&/ (Document[MagicDrawUML], OTIMOFProfileResourceExtent)](
        (error: java.lang.Throwable) => \&/.This(Vector(error)),
        (_: Unit) => \&/.That(Tuple2(d, pfResourceExtent))
      )
    }
  }

}