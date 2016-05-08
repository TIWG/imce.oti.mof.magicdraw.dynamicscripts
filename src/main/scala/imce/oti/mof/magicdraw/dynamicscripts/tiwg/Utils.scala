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
import java.lang.System
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.ui.symbols.internal.SymbolHelper._
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML._
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.utils.OTIHelper
import org.omg.oti.json.common.{OTIDocumentConfiguration, OTIDocumentSetConfiguration}
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.{MagicDrawOTIDataAdapter, MagicDrawOTIDocumentSetAdapterForDataProvider, MagicDrawOTIDocumentSetAdapterForProfileProvider, MagicDrawOTIHelper}
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api.UMLPackage
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.io.{Codec, Source}
import scala.{Boolean, Int, Long, None, Option, PartialFunction, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, augmentString, refArrayOps, require}
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

object Utils {

  class SelectableVector[U]( s: Vector[U]) {

    def select[V](pf: PartialFunction[U, V]): Vector[V] =
      s.flatMap { u => if (pf.isDefinedAt(u)) Some(pf(u)) else None }

  }

  implicit def selectable[U](s: Vector[U]): SelectableVector[U] = new SelectableVector(s)

  implicit def VectorSemigroup[T]: Semigroup[Vector[T]] =
    Semigroup.instance(_ ++ _)

  val PrimitiveTypes_IRI = Identification.LibraryIRI("http://www.omg.org/spec/PrimitiveTypes/20131001")
  val UML25_IRI = Identification.MetamodelIRI("http://www.omg.org/spec/UML/20131001")
  val StandardProfile_IRI = Identification.ProfileIRI("http://www.omg.org/spec/UML/20131001/StandardProfile")

  val resourcesPath: String = "dynamicScripts/imce.oti.mof.magicdraw.dynamicscripts/resources/"

  val MagicDraw18_UML25Implementation_DocumentSetConfiguration_File
  : String
  = resourcesPath+"MagicDraw18-UML25-implementation.documentSetConfiguration.json"

  val MagicDraw18_UML25Implementation_PrimitiveTypes_File
  : String
  = resourcesPath+ "PrimitiveTypes.library.json"

  val MagicDraw18_UML25Implementation_UMLMetamodel_File
  : String
  = resourcesPath+ "UML.metamodel.json"

  val MagicDraw18_UML25Implementation_StandardProfile_File
  : String
  = resourcesPath+"StandardProfile.profile.json"

  type OTIDocument2MOFResource =
  (Project, MagicDrawOTIDocumentSetAdapterForDataProvider, Set[OTIMOFResourceExtent]) =>
    Try[Document[MagicDrawUML] => \/[Set[java.lang.Throwable], OTIMOFResourceExtent]]

  def diagramDynamicScript
  ( p: Project,
    ev: ActionEvent,
    script: DynamicScriptsTypes.DiagramContextMenuAction,
    dpe: DiagramPresentationElement,
    triggerView: PackageView,
    triggerElement: Package,
    selection: java.util.Collection[PresentationElement],
    actionName: String,
    action: OTIDocument2MOFResource,
    chooser: () => Try[Option[(File, Set[OTIMOFResourceExtent])]])
  : Try[Option[MagicDrawValidationDataResults]]
  = chooser().flatMap {

    case None =>
      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()
      guiLog.log(s"$actionName cancelled")
      Success(None)

    case Some((otiDocumentConfigurationFile, resourceExtents)) =>
      val otiDocumentConfigurationContents
      : String
      = Source.fromFile(otiDocumentConfigurationFile)(Codec.UTF8).getLines.mkString

      val otiDocumentConfigurationJson
      = Json.parse(otiDocumentConfigurationContents)

      otiDocumentConfigurationJson.validate[OTIDocumentSetConfiguration] match {

        case e: JsError =>
          Failure(new java.lang.IllegalArgumentException(e.toString))

        case JsSuccess(otiDocumentConfigurationSet, _) =>

          OTIHelper
            .toTry(
              MagicDrawOTIHelper.getOTIMagicDrawDataAdapter(p, otiDocumentConfigurationSet),
              (oa: MagicDrawOTIDataAdapter) => {
                val app = Application.getInstance()
                val guiLog = app.getGUILog
                guiLog.clearLog()

                implicit val umlOps = oa.umlOps
                import umlOps._

                val selectedSpecificationRootPackages
                : Set[UMLPackage[Uml]]
                = selection
                  .to[Set]
                  .selectByKindOf {
                    case pv: PackageView =>
                      umlPackage(getPackageOfView(pv).get)
                  }

                val emptyConfig
                : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
                = OTIDocumentSetConfiguration.empty.that

                val t0: Long = java.lang.System.currentTimeMillis()

                val result = for {
                  odsa <- MagicDrawOTIHelper.getOTIMagicDrawDataDocumentSetAdapter(oa, selectedSpecificationRootPackages)
                  t1 = java.lang.System.currentTimeMillis()
                  _ = {
                    System.out.println(
                      s"$actionName.getOTIMagicDrawDataDocumentSetAdapter " +
                        s"in ${prettyFiniteDuration(t1 - t0, TimeUnit.MILLISECONDS)}")
                  }

                  config <- (emptyConfig /: selectedSpecificationRootPackages) {
                    addSpecificationRootPackageForDataProvider(odsa)
                  }
                  t2 = java.lang.System.currentTimeMillis()
                  _ = {
                    System.out.println(
                      s"$actionName.addSpecificationRootPackages " +
                        s"in ${prettyFiniteDuration(t2 - t1, TimeUnit.MILLISECONDS)}")
                  }
                } yield
                for {
                  cb <- action(p, odsa, resourceExtents)
                  er <- exportAsOTIMOFResource(
                    p, odsa, config,
                    selectedSpecificationRootPackages,
                    resourceExtents, cb)
                } yield er

                result.a match {
                  case None =>
                    Success(None)

                  case Some(errors) =>
                    Failure(errors.head)
                }
              })
      }
  }

  def browserDynamicScript
  ( p: Project,
    ev: ActionEvent,
    script: DynamicScriptsTypes.BrowserContextMenuAction,
    tree: Tree, node: Node,
    top: Package,
    selection: java.util.Collection[_ <: Package],
    actionName: String,
    action: OTIDocument2MOFResource,
    chooser: () => Try[Option[(File, Set[OTIMOFResourceExtent])]])
  : Try[Option[MagicDrawValidationDataResults]]
  = chooser().flatMap {

    case None =>
      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()
      guiLog.log(s"$actionName cancelled")
      Success(None)

    case Some((otiDocumentConfigurationFile, resourceExtents)) =>
      val otiDocumentConfigurationContents
      : String
      = Source.fromFile(otiDocumentConfigurationFile)(Codec.UTF8).getLines.mkString

      val otiDocumentConfigurationJson
      = Json.parse(otiDocumentConfigurationContents)

      otiDocumentConfigurationJson.validate[OTIDocumentSetConfiguration] match {

        case e: JsError =>
          Failure(new java.lang.IllegalArgumentException(e.toString))

        case JsSuccess(otiDocumentConfigurationSet, _) =>

          OTIHelper
            .toTry(
              MagicDrawOTIHelper.getOTIMagicDrawDataAdapter(p, otiDocumentConfigurationSet),
              (oa: MagicDrawOTIDataAdapter) => {
                val app = Application.getInstance()
                val guiLog = app.getGUILog
                guiLog.clearLog()

                implicit val umlOps = oa.umlOps
                import umlOps._

                val selectedSpecificationRootPackages
                : Set[UMLPackage[Uml]]
                = selection
                  .to[Set]
                  .selectByKindOf { case p: Package => umlPackage(p) }

                val emptyConfig
                : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
                = OTIDocumentSetConfiguration.empty.that

                val t0: Long = java.lang.System.currentTimeMillis()

                val result = for {
                  odsa <- MagicDrawOTIHelper.getOTIMagicDrawDataDocumentSetAdapter(oa, selectedSpecificationRootPackages)
                  t1 = java.lang.System.currentTimeMillis()
                  _ = {
                    System.out.println(
                      s"$actionName.getOTIMagicDrawDataDocumentSetAdapter " +
                        s"in ${prettyFiniteDuration(t1 - t0, TimeUnit.MILLISECONDS)}")
                  }

                  config <- (emptyConfig /: selectedSpecificationRootPackages) {
                    Utils.addSpecificationRootPackageForDataProvider(odsa)
                  }
                  t2 = java.lang.System.currentTimeMillis()
                  _ = {
                    System.out.println(
                      s"$actionName.addSpecificationRootPackages " +
                        s"in ${prettyFiniteDuration(t2 - t1, TimeUnit.MILLISECONDS)}")
                  }
                } yield
                  for {
                    cb <- action(p, odsa, resourceExtents)
                    er <- exportAsOTIMOFResource(
                      p, odsa, config,
                      selectedSpecificationRootPackages,
                      resourceExtents, cb)
                  } yield er

                result.onlyThisOrThat match {
                  case None =>
                    Success(None)
                  case Some(errorsOrResult) =>
                    errorsOrResult match {
                      case -\/(errors) =>
                        Failure(errors.head)
                      case \/-(result) =>
                        result
                    }
                }
              })
      }
  }

  def exportAsOTIMOFResource
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    config: OTIDocumentSetConfiguration,
    selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]],
    resourceExtents: Set[OTIMOFResourceExtent],
    toDocumentExtent: Document[MagicDrawUML] => \/[Set[java.lang.Throwable], OTIMOFResourceExtent])
  : Try[Option[MagicDrawValidationDataResults]]
  = {

    implicit val ops = odsa.otiAdapter.umlOps

    val app = Application.getInstance()
    val guiLog = app.getGUILog

    val t0: Long = java.lang.System.currentTimeMillis()

    val jconfig = Json.toJson(config)
    System.out.println(Json.prettyPrint(jconfig))

    val mdInstallDir = MDUML.getApplicationInstallDir.toPath

    val jsonExportZipURI = mdInstallDir.resolve(s"dynamicScripts/MagicDraw-${p.getPrimaryProjectID}.zip").toUri

    // @see http://www.oracle.com/technetwork/articles/java/compress-1565076.html
    val fos = new java.io.FileOutputStream(new java.io.File(jsonExportZipURI))
    val bos = new java.io.BufferedOutputStream(fos, 100000)
    val cos = new java.util.zip.CheckedOutputStream(bos, new java.util.zip.Adler32())
    val zos = new java.util.zip.ZipOutputStream(new java.io.BufferedOutputStream(cos))

    zos.setMethod(java.util.zip.ZipOutputStream.DEFLATED)

    var size: Int = 0
    var nb: Int = 0

    val d0: Try[Unit] = Success(())

    val dN
    : Try[Unit]
    = (d0 /: odsa.ds.allDocuments ) {
      case (Failure(t), _) =>
        Failure(t)

      case (Success(_), d) =>
        val pkg = d.scope
        if (selectedSpecificationRootPackages.contains(pkg)) {
          nb = nb + 1
          size = size + d.extent.size
          val e0: Long = java.lang.System.currentTimeMillis()

          val dN = toDocumentExtent(d)

          val e1: Long = java.lang.System.currentTimeMillis()
          System.out.println(
            s"ExportAsOTIMOFMetamodels.extent(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
              s"in ${prettyFiniteDuration(e1 - e0, TimeUnit.MILLISECONDS)}")

          dN match {
            case -\/(errors) =>
              Failure(errors.head)

            case \/-(dR) =>
              val dj = Json.toJson(dR)

              val e2: Long = java.lang.System.currentTimeMillis()
              System.out.println(
                s"ExportAsOTIMOFMetamodels.toJson(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
                  s"in ${prettyFiniteDuration(e2 - e1, TimeUnit.MILLISECONDS)}")

              val dRelativePath: String = Tag.unwrap(d.info.documentURL).stripPrefix("http://")
              val entry = new java.util.zip.ZipEntry(dRelativePath)
              zos.putNextEntry(entry)

              val s = Json.prettyPrint(dj)
              zos.write(s.getBytes(java.nio.charset.Charset.forName("UTF-8")))

              zos.closeEntry()

              val e3: Long = java.lang.System.currentTimeMillis()
              System.out.println(
                s"ExportAsOTIMOFMetamodels.pretty(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
                  s"in ${prettyFiniteDuration(e3 - e2, TimeUnit.MILLISECONDS)}")

              Success(())
          }
        } else
          Success(())
    }

    zos.close()
    val tN = java.lang.System.currentTimeMillis()
    System.out.println(
      s"ExportAsOTIMOFMetamodels.overall ($nb OTI document packages totalling $size elements) " +
        s"in ${prettyFiniteDuration(tN - t0, TimeUnit.MILLISECONDS)}")

    System.out.println(s"zip: $jsonExportZipURI")

    dN match {
      case Failure(t) =>
        Failure(t)
      case Success(_) =>
        Success(None)
    }
  }

  def chooseJsonFile
  (resourcePath: String,
   chooser: () => Try[Option[File]])
  : Try[Option[File]]
  = {
    val installRoot = Paths.get( MDUML.getInstallRoot )
    val file = installRoot.resolve(resourcePath).toFile
    if (file.exists() && file.canRead)
      Success(Some(file))
    else
      chooser()
  }

  def loadOTIMOFResourceExtent[T <: OTIMOFResourceExtent]
  (jsonFile: Option[File])
  (implicit formats: Format[T])
  : Try[Option[T]]
  = jsonFile.fold[Try[Option[T]]](Success(None)) { f =>
    val extent = Json.parse(Source.fromFile(f)(Codec.UTF8).getLines.mkString)
    extent.validate[T] match {
      case e: JsError =>
        Failure(new java.lang.IllegalArgumentException(e.toString))
      case JsSuccess(resourceExtent, _) =>
        Success(Some(resourceExtent))
    }
  }

  def chooseOTIDocumentSetConfigurationFile
  ()
  : Try[Option[File]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_DocumentSetConfiguration_File,
    () =>
      MDUML.chooseFile(
        title="Select an OTIDocumentSetConfiguration file",
        description= "*.documentSetConfiguration.json",
        fileNameSuffix=".documentSetConfiguration.json"))

  def chooseOTIDocumentSetConfigurationNoResources
  ()
  : Try[Option[(File, Set[OTIMOFResourceExtent])]]
  = chooseOTIDocumentSetConfigurationFile().map { _.map((_, Set.empty[OTIMOFResourceExtent])) }

  def choosePrimitiveTypes
  ()
  : Try[Option[OTIMOFLibraryResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_PrimitiveTypes_File,
    () => MDUML.chooseFile(
      title="Select the MD18 UML PrimitiveTypes library json file",
      description= "*.library.json",
      fileNameSuffix=".library.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFLibraryResourceExtent])

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypes
  ()
  : Try[Option[(File, Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
    } yield (ff1, Set[OTIMOFResourceExtent](ff2))

  def chooseUMLMetamodel
  ()
  : Try[Option[OTIMOFMetamodelResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_UMLMetamodel_File,
    () =>MDUML.chooseFile(
      title="Select the MD18 UML Metamodel json file",
      description= "*.metamodel.json",
      fileNameSuffix=".metamodel.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFMetamodelResourceExtent])

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel
  ()
  : Try[Option[(File, Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
    f3 <- chooseUMLMetamodel()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
    } yield (ff1, Set[OTIMOFResourceExtent](ff2, ff3))

  def chooseStandardProfile
  ()
  : Try[Option[OTIMOFProfileResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_StandardProfile_File,
    () =>MDUML.chooseFile(
      title="Select the MD18 StandardProfile profile json file",
      description= "*.profile.json",
      fileNameSuffix=".profile.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFProfileResourceExtent])

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodelAndStandardProfile
  ()
  : Try[Option[(File, Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
    f3 <- chooseUMLMetamodel()
    f4 <- chooseStandardProfile()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
      ff4 <- f4
    } yield (ff1, Set[OTIMOFResourceExtent](ff2, ff3, ff4))

  def addSpecificationRootPackageForDataProvider
  (odsa: MagicDrawOTIDocumentSetAdapterForDataProvider)
  (ri: Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration,
   p: UMLPackage[MagicDrawUML])
  : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
  = for {
    current <- ri
    pInfo <- odsa.getSpecificationRootCharacteristics(p).toThese

  } yield pInfo.fold[OTIDocumentSetConfiguration](current) { info =>
    current.copy(
      documents =
        current.documents :+ OTIDocumentConfiguration(info, p.toolSpecific_id, p.toolSpecific_url) )
  }

  def addSpecificationRootPackageForProfileProvider
  (odsa: MagicDrawOTIDocumentSetAdapterForProfileProvider)
  (ri: Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration,
   p: UMLPackage[MagicDrawUML])
  : Set[java.lang.Throwable] \&/ OTIDocumentSetConfiguration
  = for {
    current <- ri
    pInfo <- odsa.getSpecificationRootCharacteristics(p).toThese

  } yield pInfo.fold[OTIDocumentSetConfiguration](current) { info =>
    current.copy(
      documents =
        current.documents :+ OTIDocumentConfiguration(info, p.toolSpecific_id, p.toolSpecific_url) )
  }

}