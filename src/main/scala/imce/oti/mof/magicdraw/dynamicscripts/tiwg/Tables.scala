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
import java.lang.System
import java.nio.file.Path

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.util.ByteString
import com.nomagic.magicdraw.core.Project
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import org.omg.oti.mof.schema._
import play.api.libs.json._

import scala.collection.immutable._
import scala.concurrent._
import scala.{None, Option, StringContext}
import scala.Predef.String
import scala.util.{Success, Try}

object Tables {

  def resolveFile(dir: Path, filename: String)
  : File
  = dir.resolve(filename).toFile

  def fileSink[T](file: File)(implicit formats: Format[T])
  : Sink[T, Future[IOResult]]
  = Flow[T]
    .map(r => ByteString(Json.stringify(Json.toJson(r)) + "\n"))
    .toMat(FileIO.toFile(file))(Keep.right)

  def export
  (p: Project,
   resultDir: Path,
   pfs: Vector[OTIMOFProfileTables],
   pks: Vector[OTIMOFModelTables])
  : Try[Option[MagicDrawValidationDataResults]]
  = {
    implicit val system = ActorSystem("OTIMOFExport")

    val resourcesOut = Source(pfs.flatMap(_.resourceType) ++ pks.flatMap(_.resourceType))

    System.out.println(s"pf resources: ${pfs.size}")
    System.out.println(s"pk resources: ${pks.size}")
    val resourcesFile = resolveFile(resultDir, "resources.json")

    val resourcesIn = fileSink[tables.OTIMOFResourceType](resourcesFile)

    // profiles

    val extendedMetamodelsOut = Source(pfs.flatMap(_.extendedMetamodels))
    val extendedMetamodelsFile = resolveFile(resultDir, "extendedMetamodels.json")
    val extendedMetamodelsIn = fileSink[tables.profile.OTIMOFProfile2ExtendedMetamodel](extendedMetamodelsFile)

    val pfImportsOut = Source(pfs.flatMap(_.importedProfiles))
    val pfImportsFile = resolveFile(resultDir, "profileImports.json")
    val pfImportsIn = fileSink[OTIMOFResourceProfileImport](pfImportsFile)

    val libImportOut = Source(pfs.flatMap(_.importedLibraries))
    val libImportFile = resolveFile(resultDir, "libraryImports.json")
    val libImportIn = fileSink[OTIMOFResourceLibraryImport](libImportFile)

    val stereotypesOut = Source(pfs.flatMap(_.stereotypes))
    val stereotypesFile = resolveFile(resultDir, "stereotypes.json")
    val stereotypesIn = fileSink[tables.profile.OTIMOFStereotype](stereotypesFile)

    val extendedMetaclassesOut = Source(pfs.flatMap(_.extendedMetaclasses))
    val extendedMetaclassesFile = resolveFile(resultDir, "extendedMetaclasses.json")
    val extendedMetaclassesIn = fileSink[tables.profile.OTIMOFStereotype2ExtendedMetaclass](extendedMetaclassesFile)

    // models

    val metamodelsOut = Source(pks.flatMap(_.instantiatedMetamodels))
    val metamodelsFile = resolveFile(resultDir, "instantiatedMetamodels.json")
    val metamodelsIn = fileSink[OTIMOFResourceInstantiatedMetamodel](metamodelsFile)

    val appliedProfilesOut = Source(pks.flatMap(_.appliedProfiles))
    val appliedProfilesFile = resolveFile(resultDir, "appliedProfiles.json")
    val appliedProfilesIn = fileSink[OTIMOFResourceModelAppliedProfile](appliedProfilesFile)

    val elementsOut = Source(pks.flatMap(_.elements))
    val elementsFile = resolveFile(resultDir, "elements.json")
    val elementsIn = fileSink[tables.model.OTMOFModelElement](elementsFile)

    val toolSpecificElementIDsOut = Source(pks.flatMap(_.toolSpecificElementIDs))
    val toolSpecificElementIDsFile = resolveFile(resultDir, "toolSpecificElementIDs.json")
    val toolSpecificElementIDsIn = fileSink[tables.OTIMOFToolSpecificID](toolSpecificElementIDsFile)

    val toolSpecificElementURLsOut = Source(pks.flatMap(_.toolSpecificElementURLs))
    val toolSpecificElementURLsFile = resolveFile(resultDir, "toolSpecificElementURLs.json")
    val toolSpecificElementURLsIn = fileSink[tables.OTIMOFToolSpecificURL](toolSpecificElementURLsFile)

    val appliedStereotypesOut = Source(pks.flatMap(_.appliedStereotypes))
    val appliedStereotypesFile = resolveFile(resultDir, "appliedStereotypes.json")
    val appliedStereotypesIn = fileSink[tables.model.OTIMOFAppliedStereotype](appliedStereotypesFile)

    val orderedLinksOut = Source(pks.flatMap(_.orderedLinks))
    val orderedLinksFile = resolveFile(resultDir, "orderedLinks.json")
    val orderedLinksIn = fileSink[tables.model.OTIMOFModelOrderedLink](orderedLinksFile)

    val unorderedLinksOut = Source(pks.flatMap(_.unorderedLinks))
    val unorderedLinksFile = resolveFile(resultDir, "unorderedLinks.json")
    val unorderedLinksIn = fileSink[tables.model.OTIMOFModelUnorderedLink](unorderedLinksFile)

    val orderedAtomicValuesOut = Source(pks.flatMap(_.orderedAtomicValues))
    val orderedAtomicValuesFile = resolveFile(resultDir, "orderedAtomicValues.json")
    val orderedAtomicValuesIn = fileSink[tables.values.OTIMOFOrderedAttributeAtomicValue](orderedAtomicValuesFile)

    val orderedLiteralValuesOut = Source(pks.flatMap(_.orderedLiteralValues))
    val orderedLiteralValuesFile = resolveFile(resultDir, "orderedEnumerationLiteralValues.json")
    val orderedLiteralValuesIn = fileSink[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue](orderedLiteralValuesFile)

    val orderedStructuredValuesOut = Source(pks.flatMap(_.orderedStructuredValues))
    val orderedStructuredValuesFile = resolveFile(resultDir, "orderedStructuredValues.json")
    val orderedStructuredValuesIn = fileSink[tables.values.OTIMOFOrderedAttributeStructuredValueLink](orderedStructuredValuesFile)

    val unorderedAtomicValuesOut = Source(pks.flatMap(_.unorderedAtomicValues))
    val unorderedAtomicValuesFile = resolveFile(resultDir, "unorderedAtomicValues.json")
    val unorderedAtomicValuesIn = fileSink[tables.values.OTIMOFUnorderedAttributeAtomicValue](unorderedAtomicValuesFile)

    val unorderedLiteralValuesOut = Source(pks.flatMap(_.unorderedLiteralValues))
    val unorderedLiteralValuesFile = resolveFile(resultDir, "unorderedEnumerationLiteralValues.json")
    val unorderedLiteralValuesIn = fileSink[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue](unorderedLiteralValuesFile)

    val unorderedStructuredValuesOut = Source(pks.flatMap(_.unorderedStructuredValues))
    val unorderedStructuredValuesFile = resolveFile(resultDir, "unorderedStructuredValues.json")
    val unorderedStructuredValuesIn = fileSink[tables.values.OTIMOFUnorderedAttributeStructuredValueLink](unorderedStructuredValuesFile)

    // flows

    val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      // profiles

      extendedMetamodelsOut ~> extendedMetamodelsIn

      pfImportsOut ~> pfImportsIn

      libImportOut ~> libImportIn

      stereotypesOut ~> stereotypesIn

      extendedMetaclassesOut ~> extendedMetaclassesIn

      metamodelsOut ~> metamodelsIn

      appliedProfilesOut ~> appliedProfilesIn

      // models

      metamodelsOut ~> metamodelsIn

      elementsOut ~> elementsIn

      toolSpecificElementIDsOut ~> toolSpecificElementIDsIn

      toolSpecificElementURLsOut ~> toolSpecificElementURLsIn

      orderedAtomicValuesOut ~> orderedAtomicValuesIn

      orderedLiteralValuesOut ~> orderedLiteralValuesIn

      orderedStructuredValuesOut ~> orderedStructuredValuesIn

      unorderedAtomicValuesOut ~> unorderedAtomicValuesIn

      unorderedLiteralValuesOut ~> unorderedLiteralValuesIn

      unorderedStructuredValuesOut ~> unorderedStructuredValuesIn

      appliedStereotypesOut ~> appliedStereotypesIn

      orderedLinksOut ~> orderedLinksIn

      unorderedLinksOut ~> unorderedLinksIn

      ClosedShape
    })

    implicit val mat = ActorMaterializer()
    graph.run()

    Success(None)
  }
}