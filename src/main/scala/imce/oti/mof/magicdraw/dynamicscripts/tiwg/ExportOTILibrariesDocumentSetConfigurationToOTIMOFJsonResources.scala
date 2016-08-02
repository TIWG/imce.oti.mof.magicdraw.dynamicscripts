package imce.oti.mof.magicdraw.dynamicscripts.tiwg

import java.awt.event.ActionEvent
import java.lang.{IllegalArgumentException,System}
import java.nio.file.{Files,Path}

import com.nomagic.actions.NMAction
import com.nomagic.magicdraw.core.Project
import com.nomagic.uml2.ext.jmi.helpers.StereotypesHelper
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Element
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.MainToolbarMenuAction
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.internal.MDValidationAPIHelper._
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDProjectUsageHelper
import org.omg.oti.json.common.OTIDocumentSetConfiguration
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read._
import org.omg.oti.uml.read.api.{UMLElement, UMLPackage}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI.{CatalogURIMapperException, DocumentOpsException}

import scala.collection.immutable._
import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import scala.{None, Option, Some, StringContext, Tuple2}
import scala.Predef.{require,String}
import scalaz.Scalaz._
import scalaz._

object ExportOTILibrariesDocumentSetConfigurationToOTIMOFJsonResources {

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
              Utils.chooseOTIDocumentSetConfigurationNoResources,
              otiDocumentSetFile)
          }
    }

  def callback
  (resultDir: Path)
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   resourceExtents: Set[OTIMOFResourceTables],
   config: OTIDocumentSetConfiguration,
   selectedPackages: Set[UMLPackage[MagicDrawUML]])
  : Try[Option[MagicDrawValidationDataResults]]
  = {
    val mdInstallDir = MDUML.getApplicationInstallDir.toPath
    require(Files.isDirectory(mdInstallDir) && Files.isReadable(mdInstallDir))

    val otiCheck =
      StereotypesHelper.getAllProfiles(p).find { pf => pf.getName == "OTI" } match {
        case None =>
          val otiProfileFile = mdInstallDir.resolve("profiles/OMG/OMG Tool Infrastructure.mdzip").toFile
          if (!otiProfileFile.exists() || !otiProfileFile.canRead)
            Failure(new IllegalArgumentException(s"Need the OMG OTI profile (expected: $otiProfileFile)"))
          else {
            System.out.println(s"INFO: Mounting the OTI Profile...")
            MDProjectUsageHelper.attachMagicDrawLocalProject(p.getPrimaryProject, otiProfileFile).flatMap { _ =>
              Option.apply(StereotypesHelper.getProfile(p, "OTI")) match {
                case None =>
                  StereotypesHelper.getAllProfiles(p).foreach { pf =>
                    System.out.println(s"- profile: '${pf.getQualifiedName}'")
                  }
                  Failure(new IllegalArgumentException(s"Cannot find the OTI profile after mounting $otiProfileFile"))
                case Some(_) =>
                  System.out.println(s"INFO: OTI Profile has been successfully already mounted.")
                  Success(())
              }
            }
          }

        case Some(pf) =>
          System.out.println(s"INFO: OTI Profile is already mounted.")
          Success(())
      }

    otiCheck match {
      case Failure(f) =>
        Failure(f)

      case Success(_) =>

        def f
        (acc: Vector[java.lang.Throwable] \&/ Vector[OTIMOFLibraryTables],
         pkg: UMLPackage[MagicDrawUML])
        : Vector[java.lang.Throwable] \&/ Vector[OTIMOFLibraryTables]
        = {
          val inc
          = odsa.ds.lookupDocumentByScope(pkg) match {
            case None =>
              \&/.This(Vector(UMLError.illegalElementError[MagicDrawUML, UMLPackage[MagicDrawUML]](
                s"There should be a document to export to OTIMOF '${pkg.qualifiedName.get}'",
                Iterable(pkg))))

            case Some(d) =>
              ExportAsOTIMOFLibraries
                .exportAsOTIMOFLibraryTables(p, odsa, d, pkg)

          }

          acc append inc
        }

        val libraryExtents
        : Vector[java.lang.Throwable] \&/ Vector[OTIMOFLibraryTables]
        = selectedPackages.aggregate[Vector[java.lang.Throwable] \&/ Vector[OTIMOFLibraryTables]](
          \&/.That(Vector.empty)
        )(f, _ append _)

        val libs = libraryExtents.b.getOrElse(Vector.empty)

        val result = Tables.exportLibraries(resultDir, libs)

        val errors
        : Vector[java.lang.Throwable]
        = libraryExtents.a.getOrElse(Vector.empty[java.lang.Throwable])

        if (errors.isEmpty) {

          result.map(_ => None)

        } else {
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

            case (acc, ex: UMLError.UElementException[MagicDrawUML, UMLElement[MagicDrawUML]]@scala.unchecked) =>
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
