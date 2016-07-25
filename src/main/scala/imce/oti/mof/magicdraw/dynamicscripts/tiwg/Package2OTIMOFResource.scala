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

import com.nomagic.magicdraw.core.Project
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.immutable._
import scala.{StringContext, Tuple2, Unit}
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