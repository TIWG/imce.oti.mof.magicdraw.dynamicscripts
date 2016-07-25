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
package imce.oti.mof.magicdraw.dynamicscripts.tiwg.examples

import java.awt.event.ActionEvent
import java.io.File
import java.lang.System
import java.nio.file.{Files,Path}

import com.nomagic.magicdraw.core.{Application, Project}
import com.nomagic.magicdraw.core.project.ProjectDescriptorsFactory
import com.nomagic.magicdraw.openapi.uml.SessionManager
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.MainToolbarMenuAction
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML
import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML._
import gov.nasa.jpl.dynamicScripts.magicdraw.validation.MagicDrawValidationDataResults
import imce.oti.mof.magicdraw.dynamicscripts.tiwg.Utils
import org.omg.oti.json.common.OTIDocumentSetConfiguration
import org.omg.oti.magicdraw.uml.canonicalXMI.helper._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema.OTIMOFResourceExtent
import org.omg.oti.uml.read.api._

import scala.Predef.{Map => _, Set => _}
import scala.collection.immutable._
import scala.util.control.Exception._
import scala.util.{Failure, Success, Try}
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.String
import scalaz._

object ModelCreationExample {

  def createParentRelativePathsForFilename(dir: Path, relPathname: String, fileName: String)
  : Try[File]
  =  {
    val rdir = dir.resolve(relPathname)
    catching(nonFatalCatcher)
      .either({
        Files.createDirectories(rdir)
        rdir.resolve(fileName).toFile
      })
      .fold[Try[File]](
      (error: java.lang.Throwable) => Failure(error),
      (f: File) => Success(f)
    )
  }

  def example
  (p: Project, ev: ActionEvent, script: MainToolbarMenuAction)
  : Try[Option[MagicDrawValidationDataResults]]
  = if (null != p)
    Failure(new java.lang.IllegalArgumentException("This dynamic script must be executed without a project opened!"))
  else {
    val a = Application.getInstance
    val pm = a.getProjectsManager
    val root = MDUML.getApplicationInstallDir
    for {
      resultsPath <- Option.apply(System.getProperty("DYNAMIC_SCRIPTS_RESULTS_DIR")) match {
        case None =>
          System.out.println(s"No DYNAMIC_SCRIPTS_RESULTS_DIR set, using MD's installation directory instead.")
          Success(root.toPath)
        case Some(resultsLoc) =>
          val resultsDir = new File(resultsLoc)
          if (!resultsDir.exists() || !resultsDir.isDirectory || !resultsDir.canWrite)
            Failure(new java.lang.IllegalArgumentException(
              s"DYNAMIC_SCRIPTS_RESULTS_DIR='$resultsLoc' must be an existing writeable directory."))
          else
            Success(resultsDir.toPath)
      }
      templateUri = root.toPath.resolve("templates/SysML/SysML.mdzip").toUri
      _ = System.out.println(s"=> Path for MD template: $templateUri")
      exampleLoadFile <-
      createParentRelativePathsForFilename(resultsPath, "samples/SysML/created", "ModelCreationExample.mdzip")
      _ = System.out.println(s"=> Path for created MD example: $exampleLoadFile")
      exampleSaveFile <-
      createParentRelativePathsForFilename(resultsPath, "samples/SysML/updated", "ModelCreationExample.mdzip")
      _ = System.out.println(s"=> Path for updated MD example: $exampleSaveFile")

      load = ProjectDescriptorsFactory.createProjectDescriptor(templateUri)
      silent = true
      result <- catching(nonFatalCatcher)
        .either({
          // save a copy of the template project
          pm.loadProject(load, silent)
          val project = a.getProject
          val d = ProjectDescriptorsFactory.createLocalProjectDescriptor(project, exampleLoadFile)
          pm.saveProject(d, silent)
          System.out.println(s"=> Saved created MD example: $exampleLoadFile")
          project
        })
        .fold[Try[Option[MagicDrawValidationDataResults]]](
        (error: java.lang.Throwable) => Failure(error),
        (project: Project) =>
          // do the actual creation in the context of the copy of the project template
          Utils
            .mainToolbarDynamicScript(
              project, ev, script,
              "ModelCreationExample",
              exampleCallback,
              Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodel,
              Option.empty[File])
            .flatMap { result =>
              catching(nonFatalCatcher)
                .either({
                  // try to save the updated project
                  val d = ProjectDescriptorsFactory.createLocalProjectDescriptor(project, exampleSaveFile)
                  val saved = pm.saveProject(d, silent)
                  System.out.println(s"=> Saved updated MD example: $exampleSaveFile")
                  saved
                })
                .fold[Try[Option[MagicDrawValidationDataResults]]](
                (error: java.lang.Throwable) => Failure(error),
                (saved: Boolean) =>
                  if (saved)
                    Success(result)
                  else
                    Failure(new java.lang.IllegalArgumentException(
                      s"Failed to save the example project to $exampleSaveFile"))
              )
            }
      )
    } yield result
  }

  def exampleCallback
  (p: Project,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   resourceExtents: Set[OTIMOFResourceExtent],
   config: OTIDocumentSetConfiguration,
   selectedPackages: Set[UMLPackage[MagicDrawUML]] )
  : Try[Option[MagicDrawValidationDataResults]]
  = {
    import odsa.otiAdapter.umlOps._
    import odsa.otiAdapter._

    val sm = SessionManager.getInstance()

    // begin transaction
    sm.createSession(p, "Model Creation Example")

    catching(nonFatalCatcher)
      .either({
        val top: UMLModel[MagicDrawUML] = p.getModel

        System.out.println(s"Model Creation Example: ${p.getPrimaryProjectID}")
        val result
        : Set[java.lang.Throwable] \/ Unit
        = for {
          // create a new package
          pkg <- umlF.createUMLPackage

          // set its name
          _ <- umlU.set_NamedElement_name(pkg, Some("Test"))

          // make it owned by MD's root model
          _ <- umlU.Package_owningPackage_packagedElement_PackageableElement.link(top, pkg)

          // end transaction
          _ = sm.closeSession(p)
        } yield ()

        result
          .fold[Try[Option[MagicDrawValidationDataResults]]](
          (errors: Set[java.lang.Throwable]) => {
            if (sm.isSessionCreated(p)) {
              sm.cancelSession(p)
            }
            Failure(errors.head)
          },
          (_: Unit) => {
            if (sm.isSessionCreated(p)) {
              sm.cancelSession(p)
              Failure(new java.lang.IllegalArgumentException(
                s"Model Creation Example failed because the transaction is incomplete"))
            } else
              Success(None)
          }
        )
      })
      .fold[Try[Option[MagicDrawValidationDataResults]]](
        (error: java.lang.Throwable) => Failure(error),
        (result: Try[Option[MagicDrawValidationDataResults]]) => result)

  }
}