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

import com.nomagic.magicdraw.core.{Application, ApplicationEnvironment, Project}
import com.nomagic.magicdraw.core.project.{ProjectDescriptorsFactory, ProjectsManager}
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
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.util.{Failure, Success, Try}
import scalaz._

object ModelCreationExample {

  def example
  (p: Project, ev: ActionEvent, script: MainToolbarMenuAction)
  : Try[Option[MagicDrawValidationDataResults]]
  = if (null != p)
    Failure(new java.lang.IllegalArgumentException("This dynamic script must be executed without a project opened!"))
  else {
    val a = Application.getInstance
    val pm = a.getProjectsManager
    val root = MDUML.getApplicationInstallDir
    val templateUri = root.toPath.resolve("templates/SysML/SysML.mdzip").toUri
    val exampleFile = root.toPath.resolve("samples/SysML/ModelCreationExample.mdzip").toFile
    val load = ProjectDescriptorsFactory.createProjectDescriptor(templateUri)
    val silent = true
    catching(nonFatalCatcher)
      .either({
        // save a copy of the template project
        pm.loadProject(load, silent)
        val project = a.getProject
        val save = ProjectDescriptorsFactory.createLocalProjectDescriptor(project, exampleFile)
        pm.saveProject(save, silent)
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
          Utils.chooseOTIDocumentSetConfigurationAndPrimitiveTypesAndUMLMetamodelAndStandardProfile,
          Option.empty[File])
        .flatMap { result =>
          catching(nonFatalCatcher)
            .either({
              // try to save the updated project
              val save = ProjectDescriptorsFactory.createLocalProjectDescriptor(project)
              pm.saveProject(save, silent)
            })
            .fold[Try[Option[MagicDrawValidationDataResults]]](
            (error: java.lang.Throwable) => Failure(error),
            (saved: Boolean) =>
               if (saved)
                 Success(result)
               else
                 Failure(new java.lang.IllegalArgumentException(
                   s"Failed to save the example project to $exampleFile"))
          )
        }
    )
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