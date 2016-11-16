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
import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.ui.browser.{Node, Tree}
import com.nomagic.magicdraw.uml.symbols.shapes.PackageView
import com.nomagic.magicdraw.uml.symbols.{DiagramPresentationElement, PresentationElement}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Package
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes
import gov.nasa.jpl.dynamicScripts.magicdraw.ui.symbols.internal.SymbolHelper._
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import gov.nasa.jpl.imce.oti.magicdraw.dynamicScripts.utils.OTIHelper
import org.omg.oti.json.common.{OTIDocumentConfiguration, OTIDocumentSetConfiguration, OTIPrimitiveTypes}
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api.UMLPackage
import org.omg.oti.uml.xmi.Document
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.{Codec, Source}
import scala.language.higherKinds
import scala.{Boolean, Int, Long, None, Option, PartialFunction, Some, StringContext, Tuple2, Unit}
import scala.Predef.String
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom

object Utils {

  def chooseExistingDirectory
  ( title: String,
    description: String,
    dir: File = MDUML.getApplicationInstallDir )
  : Try[Option[Path]] =

    Try {
      var result: Option[Path] = None

      def chooser = new java.lang.Runnable {
        override def run(): Unit = {

          val ff = new javax.swing.filechooser.FileFilter() {

            def getDescription: String = description

            def accept(f: File): Boolean =
              f.isDirectory

          }

          val fc = new javax.swing.JFileChooser(dir) {

            override def getFileSelectionMode: Int = javax.swing.JFileChooser.DIRECTORIES_ONLY

            override def getDialogTitle = title
          }

          fc.setFileFilter(ff)
          fc.setFileHidingEnabled(true)
          fc.setAcceptAllFileFilterUsed(false)

          fc.showOpenDialog(Application.getInstance().getMainFrame) match {
            case javax.swing.JFileChooser.APPROVE_OPTION =>
              val openFile = fc.getSelectedFile
              if (openFile.isDirectory && openFile.canExecute && openFile.canRead)
                result = Some(openFile.toPath)
              else
                result = None
            case _ =>
              result = None
          }
        }
      }

      if (javax.swing.SwingUtilities.isEventDispatchThread)
        chooser.run()
      else
        javax.swing.SwingUtilities.invokeAndWait(chooser)

      result
    }

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

  val resourcesPath: String = "dynamicScripts/imce.oti.mof.magicdraw.dynamicscripts/resources/"

  val MagicDraw18_implementation_DocumentSetConfiguration_File
  : String
  = resourcesPath + "MagicDraw18-implementation.documentSetConfiguration.json"

  val MagicDraw18_UML25Implementation_PrimitiveTypes_File
  : String
  = resourcesPath + "MagicDraw18.Library.PrimitiveTypes"

  val MagicDraw18_UML25Implementation_UMLMetamodel_File
  : String
  = resourcesPath + "MagicDraw18.Metamodel.UML"

  type OTIDocument2MOFResource =
  (Project, MagicDrawOTIDocumentSetAdapterForDataProvider, Set[OTIMOFResourceTables]) =>
    Try[Document[MagicDrawUML] => \&/[Vector[java.lang.Throwable], OTIMOFResourceTables]]

  type Callback =
  (Project,
    MagicDrawOTIDocumentSetAdapterForDataProvider,
    Set[OTIMOFResourceTables],
    OTIDocumentSetConfiguration,
    Set[UMLPackage[MagicDrawUML]]) =>
    Try[Option[MagicDrawValidationDataResults]]

  def mainToolbarDynamicScript
  (p: Project,
   ev: ActionEvent,
   script: DynamicScriptsTypes.MainToolbarMenuAction,
   actionName: String,
   action: Callback,
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceTables])]],
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
                    ()

                  case Some(errors) =>
                    guiLog.log(s"Ignoring ${errors.size} errors:")
                    errors.foreach { e =>
                      System.err.println("\n\nIgnoring Error:\n")
                      System.err.println(e.getMessage)
                      e.printStackTrace(System.err)
                    }
                }

                result.b match {
                  case None =>
                    Success(None)
                  case Some(successOrError) =>
                    successOrError
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
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceTables])]])
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
   chooser: () => Try[Option[(Vector[File], Set[OTIMOFResourceTables])]])
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

  def chooseJsonFolder
  (resourcePath: String,
   chooser: () => Try[Option[Path]])
  : Try[Option[Path]]
  = {
    val installRoot = Paths.get(MDUML.getInstallRoot)
    val dir = installRoot.resolve(resourcePath).toFile
    if (dir.isDirectory && dir.exists && dir.canRead && dir.canExecute)
      Success(Some(dir.toPath))
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

  def loadOTIMOFResourceExtent[T <: OTIMOFResourceTables]
  (loader: Path => Future[(JsError, T)], timeout: Duration)
  (dir: Path)
  (implicit mat: ActorMaterializer, ec: ExecutionContext)
  : Try[Option[T]]
  = {
    val (jsError, tables) = Await.result(loader(dir), timeout)

    if (jsError == JsError())
      Success(Some(tables))

    else
      Failure(TableLoadException(s"loadOTIMOFResourceExtent(dir=$dir)", jsError))
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
  : Try[Option[(Vector[File], Set[OTIMOFResourceTables])]]
  = chooseOTIDocumentSetConfigurationFile().map { f =>
    f.map { ff =>
      Tuple2(Vector(ff), Set.empty[OTIMOFResourceTables])
    }
  }

  def choosePrimitiveTypes
  ()
  : Try[Option[OTIMOFLibraryTables]]
  = {
    implicit val system = ActorSystem()
    implicit val mat = ActorMaterializer()
    implicit val ec = system.dispatcher

    System.out.println(s"PrimitiveFilesFolder: "+MagicDraw18_UML25Implementation_PrimitiveTypes_File)
    chooseJsonFolder(
      MagicDraw18_UML25Implementation_PrimitiveTypes_File,
      () => chooseExistingDirectory(
        title = "Select the MD18 UML PrimitiveTypes library json folder",
        description = "Folder for library json files"))
      .flatMap {
        case None =>
          Success(None)

        case Some(dir) =>
          loadOTIMOFResourceExtent[OTIMOFLibraryTables](Tables.loadLibrary, Duration.Inf)(dir)
      }
  }

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypes
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceTables])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
    } yield (Vector(ff1), Set[OTIMOFResourceTables](ff2))

  def chooseUMLMetamodel
  ()
  : Try[Option[OTIMOFMetamodelTables]]
  = {
    implicit val system = ActorSystem()
    implicit val mat = ActorMaterializer()
    implicit val ec = system.dispatcher

    System.out.println(s"UML MetamodelFolder: "+MagicDraw18_UML25Implementation_UMLMetamodel_File)
    chooseJsonFolder(
      MagicDraw18_UML25Implementation_UMLMetamodel_File,
      () => chooseExistingDirectory(
        title = "Select the MD18 UML Metamodel json folder",
        description = "Folder for metamodel json files"))
      .flatMap {
        case None =>
          Success(None)

        case Some(dir) =>
          loadOTIMOFResourceExtent[OTIMOFMetamodelTables](Tables.loadMetamodel, Duration.Inf)(dir)
      }
  }

  def choosePrimitiveTypesAndUMLMetamodel4Resolver
  ()
  : Try[Option[views.UMLMetamodelResolver]]
  = for {
    pt <- choosePrimitiveTypes()
    mm <- chooseUMLMetamodel()
  } yield
    for {
      ptr <- pt
      mmr <- mm
    } yield views.UMLMetamodelResolver.initialize(ptr, mmr)

  def chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceTables])]]
  = for {
    f1 <- chooseOTIDocumentSetConfigurationFile()
    f2 <- choosePrimitiveTypes()
    f3 <- chooseUMLMetamodel()
  } yield
    for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
    } yield (Vector(ff1), Set[OTIMOFResourceTables](ff2, ff3))

  def chooseOTIDocumentSetConfigurationForUserModel
  ()
  : Try[Option[(Vector[File], Set[OTIMOFResourceTables])]]
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
    } yield (Vector(ff1, ff2), Set[OTIMOFResourceTables](ff3, ff4))

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