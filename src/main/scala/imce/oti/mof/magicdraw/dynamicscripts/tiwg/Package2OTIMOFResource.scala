package imce.oti.mof.magicdraw.dynamicscripts.tiwg

import java.io.File
import java.nio.file.Path
import java.lang.System

import com.nomagic.magicdraw.core.Project
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.immutable._
import scala.{Int, None, Option, Some, StringContext, Tuple2, Tuple4, Unit}
import scala.Predef.{String, augmentString, require}
import scala.util.control.Exception._
import scalaz._
import Scalaz._

object Package2OTIMOFResource {

  def package2OTIMOFResource
  (p: Project,
   resultDir: Path,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache,
   pfExtents: Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)],
   pkg: UMLPackage[MagicDrawUML],
   d: Document[MagicDrawUML])
  : Vector[java.lang.Throwable] \&/ (Document[MagicDrawUML], OTIMOFModelResourceExtent)
  = {
    implicit val ops = odsa.otiAdapter.umlOps
    for {
      allAppliedStereotypesByOptionalProfile <- d.scope.allAppliedStereotypesByProfile match {
        case -\/(errors) =>
          \&/.This(errors.toVector)
        case \/-(result) =>
          \&/.That(result)
      }

      modelIRI = d.toOTIMOFResourceIRI

      profiles = pfExtents.map(_._2)

      allAppliedStereotypesByProfile <-
      ExportAsOTIMOFModels.onlyAppliedStereotypesByProfile(modelIRI, allAppliedStereotypesByOptionalProfile, profiles)

      appliedStereotypes <- allAppliedStereotypesByProfile
        .aggregate[\&/[Vector[java.lang.Throwable], Vector[model.AppliedStereotype]]](\&/.That(Vector()))(
        {
          case (acc, (_, pfR, _, stereotypedElements)) =>
            import Utils.VectorSemigroup
            val inc = ExportAsOTIMOFModels.toAppliedStereotypes(pfR, stereotypedElements)
            val updated = acc append inc
            updated
        },
        _ append _)

      pkgResourceExtent <- ExportAsOTIMOFModels.exportAsOTIMOFModelExtent(
        p, odsa, pfExtents,
        d, modelIRI, appliedStereotypes, allAppliedStereotypesByProfile)(cache)

      result <- catching(nonFatalCatcher).either({

        val uriSuffix: String = OTIPrimitiveTypes.OTI_URI.unwrap(d.info.packageURI).stripPrefix("http://")
        val uriRelpath = (if (uriSuffix.endsWith("/")) uriSuffix else uriSuffix + "/").replace('/', File.separatorChar)
        val pfDir = resultDir.resolve(uriRelpath)
        require(pfDir.toFile.mkdirs(),
          s"Failed to create the directory for profile: ${pkg.qualifiedName.get} with URI=${d.info.packageURI}" +
            s" (directory=$pfDir)")

        val pfFile = pfDir.resolve("model.json").toFile

        val fos = new java.io.FileOutputStream(pfFile)
        val pw = new java.io.PrintWriter(fos)

        try {
          val pkgResourceJson = Json.toJson(pkgResourceExtent)
          pw.println(Json.prettyPrint(pkgResourceJson))

        } finally {
          pw.close()
          fos.close()
        }

      })
        .fold[Vector[java.lang.Throwable] \&/ (Document[MagicDrawUML], OTIMOFModelResourceExtent)](
        (error: java.lang.Throwable) => \&/.This(Vector(error)),
        (_: Unit) => \&/.That(Tuple2(d, pkgResourceExtent))
      )
    } yield result
  }
}
