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
package imce.oti.mof.magicdraw.dynamicscripts

import java.io.File
import java.lang.{Runnable,System}
import javax.swing.filechooser.FileFilter
import javax.swing.{JFileChooser, SwingUtilities}

import com.nomagic.magicdraw.core.Application

import org.omg.oti.uml.read.api.UMLElement
import org.omg.oti.uml.xmi.Document
import org.omg.oti.magicdraw.uml.read.MagicDrawUML

import gov.nasa.jpl.dynamicScripts.magicdraw.utils.MDUML._

import scala.util.Try
import scala.{Boolean,Option,None,Some,StringContext,Unit}
import scala.Predef.String


package object tiwg {

  implicit def toDocumentHelper(d: Document[MagicDrawUML])
  : DocumentHelper
  = new DocumentHelper(d)

  implicit def toElementHelper(e: UMLElement[MagicDrawUML])
  : ElementHelper
  = new ElementHelper(e)

  // @todo move this to MDUML.
  def chooseDirectory
  ( title: String,
    description: String,
    dir: File = getApplicationInstallDir )
  : Try[Option[File]] =

    Try {
      var result: Option[File] = None

      def chooser = new Runnable {
        override def run(): Unit = {

          val ff = new FileFilter() {

            def getDescription: String = description

            def accept(f: File): Boolean =
              f.isDirectory && f.exists()

          }

          val fc = new JFileChooser(dir)
          fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
          fc.setDialogTitle(title)

          fc.setFileFilter(ff)
          fc.setFileHidingEnabled(false)
          fc.setAcceptAllFileFilterUsed(false)
          fc.setDialogType(JFileChooser.SAVE_DIALOG)

          fc.showSaveDialog(Application.getInstance().getMainFrame) match {
            case JFileChooser.APPROVE_OPTION =>
              val dir = fc.getSelectedFile
              if (dir.exists() && dir.isDirectory && dir.canWrite) {
                System.out.println(s"Selected directory OK: $dir")
                result = Some(dir)
              } else {
                System.out.println(s"Selected directory error: $dir")
                result = None
              }
            case _ =>
              result = None
          }
        }
      }

      if (SwingUtilities.isEventDispatchThread)
        chooser.run()
      else
        SwingUtilities.invokeAndWait(chooser)

      result
    }

}