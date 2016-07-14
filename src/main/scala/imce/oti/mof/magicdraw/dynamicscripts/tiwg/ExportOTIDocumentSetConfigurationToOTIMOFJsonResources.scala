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
import java.io.File
import java.nio.file.Path
import java.lang.System

import com.nomagic.actions.NMAction
import com.nomagic.magicdraw.core.Project
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Element
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.MainToolbarMenuAction
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.internal.MDValidationAPIHelper._
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.json.common.OTIDocumentSetConfiguration
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read._
import org.omg.oti.uml.read.api.{UMLElement, UMLPackage, UMLProfile}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI.{CatalogURIMapperException, DocumentOpsException}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.util.{Failure, Success, Try}
import scala.{None, Option, Some, StringContext, Tuple2, Unit}
import scala.Predef.{ArrowAssoc, String}
import scalaz.Scalaz._
import scalaz._

object ExportOTIDocumentSetConfigurationToOTIMOFJsonResources {

  def doit
  (p: Project, ev: ActionEvent, script: MainToolbarMenuAction)
  : Try[Option[MagicDrawValidationDataResults]]
  = chooseDirectory(
    title = "Select a directory to save the exported OTI MOF Resource Json files",
    description = "Directory to save OTI MOF Resource Json files")
    .flatMap {
      case None =>
        Failure(new java.lang.IllegalArgumentException("The exporter needs a directory to write the results to"))

      case Some(resultDir) =>
        MDUML
          .chooseFile(
            title = "Select an OTIDocumentSetConfiguration file for the packages/profiles to export",
            description = "*.documentSetConfiguration.json",
            fileNameSuffix = ".documentSetConfiguration.json")
          .flatMap { otiDocumentSetFile =>

            Utils.mainToolbarDynamicScript(
              p, ev, script,
              "exportOTIDocumentSets2OMFOntologies",
              callback(resultDir.toPath),
              Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel,
              otiDocumentSetFile)
          }
    }


  def callback
  (resultDir: Path)
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   resourceExtents: Set[OTIMOFResourceExtent],
   config: OTIDocumentSetConfiguration,
   selectedPackages: Set[UMLPackage[MagicDrawUML]])
  : Try[Option[MagicDrawValidationDataResults]]
  = {
    val umlResolver =
      resourceExtents.find(Utils.PrimitiveTypes_IRI == _.resource.iri) match {
        case Some(primitiveTypesR: OTIMOFLibraryResourceExtent) =>
          resourceExtents.find(Utils.UML25_IRI == _.resource.iri) match {
            case Some(umlR: OTIMOFMetamodelResourceExtent) =>
              Some(UMLMetamodelResolver.initialize(primitiveTypesR, umlR))
            case _ =>
              None
          }
        case _ =>
          None
      }

    umlResolver match {
      case None =>
        Failure(new java.lang.IllegalArgumentException(
          "Missing MagicDraw-specific resources to create a UML resolver"))
      case Some(umlR) =>
        val cache = MetamodelTransactionPropertyNameCache(umlR)

        val (pfs, pkgs) = selectedPackages
          .foldLeft[(Vector[UMLProfile[MagicDrawUML]], Vector[UMLPackage[MagicDrawUML]])](
          Tuple2(Vector.empty, Vector.empty)
        ) {
          case ((profiles, packages), pf: UMLProfile[MagicDrawUML]) =>
            Tuple2(profiles :+ pf, packages)
          case ((profiles, packages), pkg) =>
            Tuple2(profiles, packages :+ pkg)
        }

        val (ppfs, ppkgs) = (pfs.par, pkgs.par)
        ppfs.tasksupport =
          new scala.collection.parallel.ForkJoinTaskSupport(
            new scala.concurrent.forkjoin.ForkJoinPool(Utils.poolSize))
        ppkgs.tasksupport =
          new scala.collection.parallel.ForkJoinTaskSupport(
            new scala.concurrent.forkjoin.ForkJoinPool(Utils.poolSize))

//        val profileExtents
//        : Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)]
//        = pfs.foldLeft[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)]](
//          \&/.That(Vector.empty)
//        ) { case (acc, pf) =>
//
//          val inc =
//            odsa.ds.lookupDocumentByScope(pf) match {
//              case None =>
//                \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
//                  s"There should be a document to export to OTIMOF '${pf.qualifiedName.get}'",
//                  Iterable(pf))))
//
//              case Some(d) =>
//                Profile2OTIMOFResource
//                  .profile2OTIMOFResource(resultDir, odsa, umlR, pf, d)
//                  .map(Vector(_))
//            }
//
//          acc append inc
//        }
        val profileExtents
        : Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)]
        = ppfs.aggregate[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)]](
          \&/.That(Vector.empty)
        )( { case (acc, pf) =>

          val inc =
            odsa.ds.lookupDocumentByScope(pf) match {
              case None =>
                \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
                  s"There should be a document to export to OTIMOF '${pf.qualifiedName.get}'",
                  Iterable(pf))))

              case Some(d) =>
                Profile2OTIMOFResource
                  .profile2OTIMOFResource(resultDir, odsa, umlR, pf, d)
                  .map(Vector(_))
            }

          acc append inc
        }, _ append _)

//        val packageExtents
//        : Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFModelResourceExtent)]
//        = profileExtents.flatMap { pfExtents =>
//          pkgs.foldLeft[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFModelResourceExtent)]](
//            \&/.That(Vector.empty)
//          ) { case (acc, pkg) =>
//
//            val inc
//            = odsa.ds.lookupDocumentByScope(pkg) match {
//              case None =>
//                \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
//                  s"There should be a document to export to OTIMOF '${pkg.qualifiedName.get}'",
//                  Iterable(pkg))))
//
//              case Some(d) =>
//                Package2OTIMOFResource
//                  .package2OTIMOFResource(p, resultDir, odsa, cache, pfExtents, pkg, d)
//                  .map(Vector(_))
//
//            }
//
//            acc append inc
//          }
//        }
        val packageExtents
        : Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFModelResourceExtent)]
        = profileExtents.flatMap { pfExtents =>
          ppkgs.aggregate[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFModelResourceExtent)]](
            \&/.That(Vector.empty)
          )( { case (acc, pkg) =>

            val inc
            = odsa.ds.lookupDocumentByScope(pkg) match {
              case None =>
                \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
                  s"There should be a document to export to OTIMOF '${pkg.qualifiedName.get}'",
                  Iterable(pkg))))

              case Some(d) =>
                Package2OTIMOFResource
                  .package2OTIMOFResource(p, resultDir, odsa, cache, pfExtents, pkg, d)
                  .map(Vector(_))

            }

            acc append inc
          }, _ append _)
        }

        val errors
        : Vector[java.lang.Throwable]
        = packageExtents.a.getOrElse(Vector.empty[java.lang.Throwable]) ++
          profileExtents.a.getOrElse(Vector.empty[java.lang.Throwable])

        if (errors.isEmpty)
          Success(None)
        else {
          val model = p.getModel
          val element2messages
          : Map[Element, (String, List[NMAction])]
          = errors.foldLeft[Map[Element, (String, List[NMAction])]](
            Map.empty
          ) {
            case (acc, ex: DocumentOpsException[_]) =>
              acc.updated(
                model,
                acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })

            case (acc, ex: CatalogURIMapperException) =>
              acc.updated(
                model,
                acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })

            case (acc, ex: UMLError.UMLAdaptationError) =>
              acc.updated(
                model,
                acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })

            case (acc, ex: UMLError.UMLAdaptationException) =>
              acc.updated(
                model,
                acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })

            case (acc, ex: UMLError.UMLOpsError[_]) =>
              acc.updated(
                model,
                acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })

            case (acc, ex: UMLError.UMLOpsException[_]) =>
            acc.updated(
              model,
              acc.getOrElse(model, Tuple2("", List.empty)) match { case (message, actions) =>
                Tuple2(message + "\n" + ex.getMessage, actions)
              })

            case (acc, ex: UMLError.UElementException[MagicDrawUML, UMLElement[MagicDrawUML]] @scala.unchecked) =>
              val e = odsa.otiAdapter.umlOps.umlMagicDrawUMLElement(ex.element.head).getMagicDrawElement
              acc.updated(
                e,
                acc.getOrElse(e, Tuple2("", List.empty)) match { case (message, actions) =>
                  Tuple2(message + "\n" + ex.getMessage, actions)
                })
          }

          Success(Some(
            p.makeMDIllegalArgumentExceptionValidation(
              s"*** ${errors.size} errors occured when exporting to OTI MOF Resources",
              element2messages,
              "*::MagicDrawOTIValidation",
              "*::UnresolvedCrossReference").validationDataResults))
        }
    }
  }

}