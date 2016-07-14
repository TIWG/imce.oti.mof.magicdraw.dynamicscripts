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
import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.json.common.{OTIDocumentConfiguration, OTIDocumentSetConfiguration, OTIPrimitiveTypes}
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api.{UMLPackage, UMLProfile}
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.io.{Codec, Source}
import scala.language.higherKinds
import scala.{Boolean, Int, Long, None, Option, PartialFunction, Some, StringContext, Tuple2, Unit}
import scala.Predef.{ArrowAssoc, String, augmentString, refArrayOps, require}
import scala.util.{Failure, Success, Try}
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom

object Utils {

  // for Scala parallel collections.
  val poolSize: Int = 4

  /**
    * OCL-like select (filter) + collect (downcast)
    *
    * OCL:
    * {{{
    * s->select(oclIsKindOf(V))->collect(oclAsType(V))
    * }}}
    *
    * Scala:
    *
    * {{{
    * import Utils.selectable
    *
    * s.select { case v: V => v }
    * }}}
    *
    * @param s A collection of type C[U]
    * @tparam U A type of elements
    * @tparam C A collection type
    */
  class Selectable[U, C[X <: U] <: GenTraversable[X]](s: C[U]) {

    def select[V <: U]
    (pf: PartialFunction[U, V])
    (implicit bf: CanBuildFrom[C[U], V, C[V]])
    : C[V]
    = {
      val b = bf(s)
      s.foreach { u: U =>
        if (pf.isDefinedAt(u)) {
          b += pf(u)
        }
        ()
      }
      b.result
    }

  }

  implicit def selectable[U, C[X <: U] <: GenTraversable[X]](s: C[U])
  : Selectable[U, C]
  = new Selectable[U, C](s)

  implicit def IterableSemigroup[T]
  : Semigroup[Iterable[T]]
  = Semigroup.instance(_ ++ _)

  implicit def VectorSemigroup[T]
  : Semigroup[Vector[T]]
  = Semigroup.instance(_ ++ _)

  val PrimitiveTypes_IRI = common.ResourceIRI("http://www.omg.org/spec/PrimitiveTypes/20131001")
  val UML25_IRI = common.ResourceIRI("http://www.omg.org/spec/UML/20131001")
  val StandardProfile_IRI = common.ResourceIRI("http://www.omg.org/spec/UML/20131001/StandardProfile")
  val SysMLProfile_IRI = common.ResourceIRI("http://www.omg.org/spec/SysML/20131201/SysML")

  val resourcesPath: String = "dynamicScripts/imce.oti.mof.magicdraw.dynamicscripts/resources/"

  val MagicDraw18_implementation_DocumentSetConfiguration_File
  : String
  = resourcesPath + "MagicDraw18-implementation.documentSetConfiguration.json"

  val MagicDraw18_UML25Implementation_PrimitiveTypes_File
  : String
  = resourcesPath + "PrimitiveTypes.library.json"

  val MagicDraw18_UML25Implementation_UMLMetamodel_File
  : String
  = resourcesPath + "UML.metamodel.json"

  val MagicDraw18_SysML14Implementation_SysML_File
  : String
  = resourcesPath + "SysML.profile.json"

  val MagicDraw18_UML25Implementation_StandardProfile_File
  : String
  = resourcesPath + "StandardProfile.profile.json"

  type OTIDocument2MOFResource =
  (Project, MagicDrawOTIDocumentSetAdapterForDataProvider, Set[OTIMOFResourceExtent]) =>
    Try[Document[MagicDrawUML] => \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent]]

  type Callback =
  (Project,
    MagicDrawOTIDocumentSetAdapterForDataProvider,
    Set[OTIMOFResourceExtent],
    OTIDocumentSetConfiguration,
    Set[UMLPackage[MagicDrawUML]]) =>
    Try[Option[MagicDrawValidationDataResults]]

  def mainToolbarDynamicScript
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.MainToolbarMenuAction,
   actionName: String,
   action: Callback,
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]],
   otiDocumentSetFile: Option[File])
  : Try[Option[MagicDrawValidationDataResults]]
  = chooser().flatMap {

    case None =>
      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()
      guiLog.log(s"$actionName cancelled")
      Success(None)

    case Some((otiDocumentConfigurationFiles, resourceExtents)) =>

      val otiDocumentConfigurationContents
      : Vector[String]
      = otiDocumentConfigurationFiles.map(Source.fromFile(_)(Codec.UTF8).getLines.mkString).toVector

      val otiDocumentConfigurationJs
      : Vector[JsResult[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationContents.map { s =>
        Json.parse(s).validate[OTIDocumentSetConfiguration]
      }

      val otiDocumentConfigurationsOrErrors
      : Try[Vector[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationJs.foldLeft[Try[Vector[OTIDocumentSetConfiguration]]](Success(Vector())) {
        case (_, e: JsError) =>
          Failure(new java.lang.IllegalArgumentException(e.toString))

        case (Failure(t), _) =>
          Failure(t)

        case (Success(acc), JsSuccess(otiDocumentConfigurationSet, _)) =>
          Success(acc :+ otiDocumentConfigurationSet)
      }

      otiDocumentConfigurationsOrErrors.flatMap { otiDocumentConfigurations =>

        loadOTIDocumentSet(otiDocumentSetFile).flatMap { documentSelection =>

          OTIHelper
            .toTry(
              MagicDrawOTIHelper.getOTIMagicDrawDataAdapter(p, otiDocumentConfigurations ++ documentSelection),
              (oa: MagicDrawOTIDataAdapter) => {
                val app = Application.getInstance()
                val guiLog = app.getGUILog
                guiLog.clearLog()

                implicit val umlOps = oa.umlOps

                val init
                : Set[java.lang.Throwable] \&/ Set[UMLPackage[MagicDrawUML]]
                = Set.empty[UMLPackage[MagicDrawUML]].that

                val config = documentSelection.getOrElse(OTIDocumentSetConfiguration.empty)

                val result = for {
                  odsa1 <- MagicDrawOTIAdapters.withInitialDocumentSetForDataAdapter(oa)

                  selectedSpecificationRootPackages <- (init /: config.documents) {
                    lookupAndAddDocumentPackage(p, odsa1)
                  }

                  odsa2 <- MagicDrawOTIHelper.getOTIMagicDrawDataDocumentSetAdapter(
                    oa, selectedSpecificationRootPackages, MagicDrawOTIHelper.defaultExtentOfPkg)
                } yield
                  for {
                    er <- action(
                      p, odsa2, resourceExtents, config,
                      selectedSpecificationRootPackages)
                  } yield er

                result.a match {
                  case None =>
                    result.b match {
                      case None =>
                        Success(None)
                      case Some(successOrError) =>
                        successOrError
                    }

                  case Some(errors) =>
                    Failure(errors.head)
                }
              })
        }
      }
  }

  def diagramDynamicScript
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.DiagramContextMenuAction,
   dpe: DiagramPresentationElement,
   triggerView: PackageView,
   triggerElement: Package,
   selection: java.util.Collection[PresentationElement],
   actionName: String,
   action: Callback,
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]])
  : Try[Option[MagicDrawValidationDataResults]]
  = chooser().flatMap {

    case None =>
      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()
      guiLog.log(s"$actionName cancelled")
      Success(None)

    case Some((otiDocumentConfigurationFiles, resourceExtents)) =>

      val otiDocumentConfigurationContents
      : Vector[String]
      = otiDocumentConfigurationFiles.map(Source.fromFile(_)(Codec.UTF8).getLines.mkString).toVector

      val otiDocumentConfigurationJs
      : Vector[JsResult[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationContents.map { s =>
        Json.parse(s).validate[OTIDocumentSetConfiguration]
      }

      val otiDocumentConfigurationsOrErrors
      : Try[Vector[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationJs.foldLeft[Try[Vector[OTIDocumentSetConfiguration]]](Success(Vector())) {
        case (_, e: JsError) =>
          Failure(new java.lang.IllegalArgumentException(e.toString))

        case (Failure(t), _) =>
          Failure(t)

        case (Success(acc), JsSuccess(otiDocumentConfigurationSet, _)) =>
          Success(acc :+ otiDocumentConfigurationSet)
      }

      otiDocumentConfigurationsOrErrors.flatMap { otiDocumentConfigurations =>

        OTIHelper
          .toTry(
            MagicDrawOTIHelper.getOTIMagicDrawDataAdapter(p, otiDocumentConfigurations),
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
                odsa <- MagicDrawOTIHelper.getOTIMagicDrawDataDocumentSetAdapter(
                  oa, selectedSpecificationRootPackages, MagicDrawOTIHelper.defaultExtentOfPkg)

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
                  er <- action(
                    p, odsa, resourceExtents, config,
                    selectedSpecificationRootPackages)
                } yield er

              result.a match {
                case None =>
                  result.b match {
                    case None =>
                      Success(None)
                    case Some(r) =>
                      r
                  }

                case Some(errors) =>
                  Failure(errors.head)
              }
            })
      }
  }

  def browserDynamicScript
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.BrowserContextMenuAction,
   tree: Tree, node: Node,
   top: Package,
   selection: java.util.Collection[_ <: Package],
   actionName: String,
   action: Callback,
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]])
  : Try[Option[MagicDrawValidationDataResults]]
  = chooser().flatMap {

    case None =>
      val app = Application.getInstance()
      val guiLog = app.getGUILog
      guiLog.clearLog()
      guiLog.log(s"$actionName cancelled")
      Success(None)

    case Some((otiDocumentConfigurationFiles, resourceExtents)) =>

      val otiDocumentConfigurationContents
      : Vector[String]
      = otiDocumentConfigurationFiles.map(Source.fromFile(_)(Codec.UTF8).getLines.mkString).toVector

      val otiDocumentConfigurationJs
      : Vector[JsResult[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationContents.map { s =>
        Json.parse(s).validate[OTIDocumentSetConfiguration]
      }

      val otiDocumentConfigurationsOrErrors
      : Try[Vector[OTIDocumentSetConfiguration]]
      = otiDocumentConfigurationJs.foldLeft[Try[Vector[OTIDocumentSetConfiguration]]](Success(Vector())) {
        case (_, e: JsError) =>
          Failure(new java.lang.IllegalArgumentException(e.toString))

        case (Failure(t), _) =>
          Failure(t)

        case (Success(acc), JsSuccess(otiDocumentConfigurationSet, _)) =>
          Success(acc :+ otiDocumentConfigurationSet)
      }

      otiDocumentConfigurationsOrErrors.flatMap { otiDocumentConfigurations =>

        OTIHelper
          .toTry(
            MagicDrawOTIHelper.getOTIMagicDrawDataAdapter(p, otiDocumentConfigurations),
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
                odsa <- MagicDrawOTIHelper.getOTIMagicDrawDataDocumentSetAdapter(
                  oa, selectedSpecificationRootPackages, MagicDrawOTIHelper.defaultExtentOfPkg)

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
                  er <- action(
                    p, odsa, resourceExtents, config,
                    selectedSpecificationRootPackages)
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

  def serializeOTIMOFResourceExtent2JSon
  (zos: java.util.zip.ZipOutputStream,
   actionName: String,
   e1: Long,
   d: Document[MagicDrawUML],
   dr: OTIMOFResourceExtent)
  : \&/[Vector[java.lang.Throwable], Unit]
  = catching(nonFatalCatcher)
    .either({
      val dj = Json.toJson(dr)

      val e2: Long = java.lang.System.currentTimeMillis()
      System.out.println(
        s"$actionName.toJson(${d.scope.qualifiedName.get} with ${d.extent.size} elements) " +
          s"in ${prettyFiniteDuration(e2 - e1, TimeUnit.MILLISECONDS)}")

      val dRelativePath: String = Tag.unwrap(d.info.documentURL).stripPrefix("http://")
      val entry = new java.util.zip.ZipEntry(dRelativePath)
      zos.putNextEntry(entry)

      val s = Json.prettyPrint(dj)
      zos.write(s.getBytes(java.nio.charset.Charset.forName("UTF-8")))

      zos.closeEntry()

      val e3: Long = java.lang.System.currentTimeMillis()
      System.out.println(
        s"$actionName.pretty(${d.scope.qualifiedName.get} with ${d.extent.size} elements) " +
          s"in ${prettyFiniteDuration(e3 - e2, TimeUnit.MILLISECONDS)}")
    })
    .fold[\&/[Vector[java.lang.Throwable], Unit]](
    (error: java.lang.Throwable) => \&/.This(Vector(error)),
    (_: Unit) => \&/.That(()))

  def exportAsOTIMOFResource
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   config: OTIDocumentSetConfiguration,
   selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]],
   resourceExtents: Set[OTIMOFResourceExtent],
   toDocumentExtent: (Document[MagicDrawUML], Set[Document[MagicDrawUML]]) => \&/[Vector[java.lang.Throwable], OTIMOFResourceExtent],
   actionName: String)
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

    val errors = new scala.collection.mutable.ListBuffer[java.lang.Throwable]()

    val pfDocuments = odsa.ds.allDocuments.flatMap { d =>
      d.scope match {
        case _: UMLProfile[MagicDrawUML] =>
          Some(d)
        case _ =>
          None
      }
    }

    odsa.ds.allDocuments.foreach { d =>

      val pkg = d.scope
      if (selectedSpecificationRootPackages.contains(pkg)) {
        nb = nb + 1
        size = size + d.extent.size
        val e0: Long = java.lang.System.currentTimeMillis()

        val dN = toDocumentExtent(d, pfDocuments)

        val e1: Long = java.lang.System.currentTimeMillis()
        System.out.println(
          s"$actionName.extent(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
            s"in ${prettyFiniteDuration(e1 - e0, TimeUnit.MILLISECONDS)}")

        dN.b.foreach { dR =>
          val dj = Json.toJson(dR)

          val e2: Long = java.lang.System.currentTimeMillis()
          System.out.println(
            s"$actionName.toJson(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
              s"in ${prettyFiniteDuration(e2 - e1, TimeUnit.MILLISECONDS)}")

          val dRelativePath: String = Tag.unwrap(d.info.documentURL).stripPrefix("http://")
          val entry = new java.util.zip.ZipEntry(dRelativePath)
          zos.putNextEntry(entry)

          val s = Json.prettyPrint(dj)
          zos.write(s.getBytes(java.nio.charset.Charset.forName("UTF-8")))

          zos.closeEntry()

          val e3: Long = java.lang.System.currentTimeMillis()
          System.out.println(
            s"$actionName.pretty(${pkg.qualifiedName.get} with ${d.extent.size} elements) " +
              s"in ${prettyFiniteDuration(e3 - e2, TimeUnit.MILLISECONDS)}")
        }

        dN.a.map { dErrors =>
          errors ++= dErrors
        }
      }
    }

    zos.close()
    val tN = java.lang.System.currentTimeMillis()
    System.out.println(
      s"$actionName.overall ($nb OTI document packages totalling $size elements) " +
        s"in ${prettyFiniteDuration(tN - t0, TimeUnit.MILLISECONDS)}")

    System.out.println(s"zip: $jsonExportZipURI")

    guiLog.log(s"Exported OTI MOF Model as $jsonExportZipURI")
    guiLog.log(s"${errors.size} errors")

    System.err.println(s"\n***\n${errors.size} errors\n****")
    errors.foreach { e =>
      System.err.println(e)
      System.err.println()
    }
    System.err.println(s"\n*******\n")

    Success(None)
  }

  def exportAsOTIMOFProfiledResources
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   config: OTIDocumentSetConfiguration,
   selectedSpecificationRootPackages: Set[UMLPackage[MagicDrawUML]],
   resourceExtents: Set[OTIMOFResourceExtent],
   toDocumentProfileExtent: (Document[MagicDrawUML], Set[Document[MagicDrawUML]]) => \&/[Vector[java.lang.Throwable], OTIMOFProfileResourceExtent],
   toDocumentModelExtent: (Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)], Document[MagicDrawUML], Set[Document[MagicDrawUML]]) => \&/[Vector[java.lang.Throwable], OTIMOFModelResourceExtent],
   actionName: String)
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

    val selectedPFs = selectedSpecificationRootPackages.flatMap {
      case pf: UMLProfile[MagicDrawUML] =>
        Some(pf)
      case _ =>
        None
    }

    val pfDocuments = odsa.ds.allDocuments.flatMap { d =>
      d.scope match {
        case _: UMLProfile[MagicDrawUML] =>
          Some(d)
        case _ =>
          None
      }
    }

    val pfResourcesOrErrors =
      pfDocuments
        .foldLeft[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)]](
        \&/.That(Vector.empty[(Document[MagicDrawUML], OTIMOFProfileResourceExtent)])
      ) { case (acc, d) =>
        d.scope match {
          case pf: UMLProfile[MagicDrawUML] if selectedPFs.contains(pf) =>
            nb = nb + 1
            size = size + d.extent.size
            val e0: Long = java.lang.System.currentTimeMillis()

            val inc = toDocumentProfileExtent(d, pfDocuments).map(r => Vector((d, r)))

            acc append inc

          case _ =>
            acc
        }
      }

    val pfResources = pfResourcesOrErrors.b.getOrElse(Vector())

    val selectedPKGs = selectedSpecificationRootPackages.flatMap {
      case _: UMLProfile[MagicDrawUML] =>
        None
      case pkg: UMLPackage[MagicDrawUML] =>
        Some(pkg)
    }

    val pkgDocuments = odsa.ds.allDocuments.flatMap { d =>
      d.scope match {
        case _: UMLProfile[MagicDrawUML] =>
          None
        case _ =>
          Some(d)
      }
    }

    val pkgResourcesOrErrors =
      odsa.ds.allDocuments
        .foldLeft[Vector[java.lang.Throwable] \&/ Vector[(Document[MagicDrawUML], OTIMOFModelResourceExtent)]](
        \&/.That(Vector.empty[(Document[MagicDrawUML], OTIMOFModelResourceExtent)])
      ) { case (acc, d) =>
        d.scope match {
          case _: UMLProfile[MagicDrawUML] =>
            acc
          case pkg: UMLPackage[MagicDrawUML] if selectedPKGs.contains(pkg) =>
            nb = nb + 1
            size = size + d.extent.size
            val e0: Long = java.lang.System.currentTimeMillis()

            val inc = toDocumentModelExtent(pfResources, d, pkgDocuments).map(r => Vector((d, r)))

            acc append inc

          case _ =>
            acc
        }
      }

    val pkgResources = pkgResourcesOrErrors.b.getOrElse(Vector())

    var e1: Long = java.lang.System.currentTimeMillis()

    val saved
    : Vector[java.lang.Throwable] \&/ Unit
    = (pfResources ++ pkgResources).foldLeft[Vector[java.lang.Throwable] \&/ Unit](\&/.That(())) { case (acc, (d,r)) =>
      e1 = java.lang.System.currentTimeMillis()
      val inc = serializeOTIMOFResourceExtent2JSon(zos, actionName, e1, d, r)
      acc append inc
    }

    zos.close()
    val tN = java.lang.System.currentTimeMillis()
    System.out.println(
      s"$actionName.overall ($nb OTI document packages totalling $size elements) " +
        s"in ${prettyFiniteDuration(tN - t0, TimeUnit.MILLISECONDS)}")

    System.out.println(s"zip: $jsonExportZipURI")

    val errors =
      pfResourcesOrErrors.a.getOrElse(Vector()) ++
        pfResourcesOrErrors.a.getOrElse(Vector()) ++
        saved.a.getOrElse(Vector())

    guiLog.log(s"Exported OTI MOF Model as $jsonExportZipURI")
    guiLog.log(s"${errors.size} errors")

    System.err.println(s"\n***\n${errors.size} errors\n****")
    errors.foreach { e =>
      System.err.println(e)
      System.err.println()
    }
    System.err.println(s"\n*******\n")

    Success(None)
  }

  def chooseJsonFile
  (resourcePath: String,
   chooser: () => Try[Option[File]])
  : Try[Option[File]]
  = {
    val installRoot = Paths.get(MDUML.getInstallRoot)
    val file = installRoot.resolve(resourcePath).toFile
    if (file.exists() && file.canRead)
      Success(Some(file))
    else
      chooser()
  }

  def loadOTIDocumentSet
  (jsonFile: Option[File])
  (implicit formats: Format[OTIDocumentSetConfiguration])
  : Try[Option[OTIDocumentSetConfiguration]]
  = jsonFile
    .fold[Try[Option[OTIDocumentSetConfiguration]]](Success(None)) { f =>
    val extent = Json.parse(Source.fromFile(f)(Codec.UTF8).getLines.mkString)
    extent.validate[OTIDocumentSetConfiguration] match {
      case e: JsError =>
        Failure(new java.lang.IllegalArgumentException(e.toString))
      case JsSuccess(ds, _) =>
        Success(Some(ds))
    }
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
    MagicDraw18_implementation_DocumentSetConfiguration_File,
    () =>
      MDUML.chooseFile(
        title = "Select an OTIDocumentSetConfiguration file",
        description = "*.documentSetConfiguration.json",
        fileNameSuffix = ".documentSetConfiguration.json"))

  def chooseOTIDocumentSetConfigurationNoResources
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]]
  = chooseOTIDocumentSetConfigurationFile().map { f =>
    f.map { ff =>
      Tuple2(Vector(ff), Set.empty[OTIMOFResourceExtent])
    }
  }

  def choosePrimitiveTypes
  ()
  : Try[Option[OTIMOFLibraryResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_PrimitiveTypes_File,
    () => MDUML.chooseFile(
      title = "Select the MD18 UML PrimitiveTypes library json file",
      description = "*.library.json",
      fileNameSuffix = ".library.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFLibraryResourceExtent])

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypes
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
    } yield (Vector(ff1), Set[OTIMOFResourceExtent](ff2))

  def chooseUMLMetamodel
  ()
  : Try[Option[OTIMOFMetamodelResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_UMLMetamodel_File,
    () => MDUML.chooseFile(
      title = "Select the MD18 UML Metamodel json file",
      description = "*.metamodel.json",
      fileNameSuffix = ".metamodel.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFMetamodelResourceExtent])

  def choosePrimitiveTypesAndUMLMetamodel4Resolver
  ()
  : Try[Option[UMLMetamodelResolver]]
  = for {
    pt <- choosePrimitiveTypes()
    mm <- chooseUMLMetamodel()
  } yield
    for {
      ptr <- pt
      mmr <- mm
    } yield UMLMetamodelResolver.initialize(ptr, mmr)

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
    f3 <- chooseUMLMetamodel()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
    } yield (Vector(ff1), Set[OTIMOFResourceExtent](ff2, ff3))

  def chooseStandardProfile
  ()
  : Try[Option[OTIMOFProfileResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_UML25Implementation_StandardProfile_File,
    () => MDUML.chooseFile(
      title = "Select the MD18 StandardProfile profile json file",
      description = "*.profile.json",
      fileNameSuffix = ".profile.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFProfileResourceExtent])

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodelAndStandardProfile
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]]
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
    } yield (Vector(ff1), Set[OTIMOFResourceExtent](ff2, ff3, ff4))

  def chooseSysML
  ()
  : Try[Option[OTIMOFProfileResourceExtent]]
  = chooseJsonFile(
    MagicDraw18_SysML14Implementation_SysML_File,
    () => MDUML.chooseFile(
      title = "Select the MD18 SysML profile json file",
      description = "*.profile.json",
      fileNameSuffix = ".profile.json"))
    .flatMap(loadOTIMOFResourceExtent[OTIMOFProfileResourceExtent])

  def chooseOTIDocumentSetConfigurationForUserModel
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceExtent])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- MDUML.chooseFile(
      title = "Select an OTIDocumentSetConfiguration file for exporting model packages",
      description = "*.documentSetConfiguration.json",
      fileNameSuffix = ".documentSetConfiguration.json")
    f3 <- choosePrimitiveTypes()
    f4 <- chooseUMLMetamodel()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
      ff4 <- f4
    } yield (Vector(ff1, ff2), Set[OTIMOFResourceExtent](ff3, ff4))

  def lookupAndAddDocumentPackage
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider)
  (current: Set[java.lang.Throwable] \&/ Set[UMLPackage[MagicDrawUML]],
   d: OTIDocumentConfiguration)
  : Set[java.lang.Throwable] \&/ Set[UMLPackage[MagicDrawUML]]
  = for {
    pkgs <- current
    id = OTIPrimitiveTypes.TOOL_SPECIFIC_ID.unwrap(d.toolSpecificPackageID)
    pkg <- p.getElementByID(id) match {
      case p: Package =>
        \&/.That(odsa.documentOps.umlOps.umlPackage(p))
      case _ =>
        \&/.This(Set[java.lang.Throwable](UMLError.umlAdaptationError(s"No MD package/profile found with ID=$id")))
    }
  } yield pkgs + pkg

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
        current.documents :+ OTIDocumentConfiguration(info, p.toolSpecific_id, p.toolSpecific_url))
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
        current.documents :+ OTIDocumentConfiguration(info, p.toolSpecific_id, p.toolSpecific_url))
  }

}