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
package imce.oti.mof.resolvers

import imce.oti.mof.magicdraw.dynamicscripts.tiwg._
import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api.{UMLClass, UMLDataType, UMLEnumeration, UMLPrimitiveType, UMLProperty}
import org.omg.oti.uml.UMLError

import scala.collection.immutable.Iterable
import scala.{AnyVal,Some,StringContext}

object OTIHelpers {

  implicit def toOTIPropertyHelper(p: UMLProperty[MagicDrawUML])
  : OTIPropertyHelper
  = new OTIPropertyHelper(p)

  class OTIPropertyHelper(val p: UMLProperty[MagicDrawUML]) extends AnyVal {

    def getMetaClassUUID()
    : common.EntityUUID
    = p._type match {
      case Some(mc: UMLClass[MagicDrawUML]) =>
        mc.toOTIMOFEntityUUID
      case _ =>
        throw UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
          s"Property should be typed by a UMLClass[MagicDrawUML]",
          Iterable(p))
    }

    def getSchemaDatatypeUUID()
    : common.EntityUUID
    = p._type match {
      case Some(dt: UMLDataType[MagicDrawUML]) =>
        dt match {
          case t: UMLPrimitiveType[MagicDrawUML] =>
            t.toOTIMOFEntityUUID
          case t: UMLEnumeration[MagicDrawUML] =>
            t.toOTIMOFEntityUUID
          case t =>
            t.toOTIMOFEntityUUID
        }
      case _ =>
        throw UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
        s"Property should be typed by a UMLDataType[MagicDrawUML]",
        Iterable(p))
    }
  }
}