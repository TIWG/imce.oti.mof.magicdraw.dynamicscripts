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

import org.omg.oti.magicdraw.uml.read.MagicDrawUML
import org.omg.oti.mof.schema._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._

import scala.collection.immutable._
import scala.{None, Some, StringContext}
import scalaz._

/**
  * Export selected UML Profiles as OTI MOF Profiles, that is, the exported OTIMOFProfileResourceExtents
  * will only have the UML Stereotypes & stereotype associations corresponding to an OTI MOF profile,
  * which is a restricted kind of OMG UML 2.5 profile without profile-defined datatypes or classes.
  *
  * Everything outside the scope of an OTIMOFProfileResourceExtent is ignored
  */
object ExportAsOTIMOFProfiles {

  import Utils.VectorSemigroup

  def getStereotype2ExtendedMetaclasses
  ( profileIRI: common.ResourceIRI,
    ss: Vector[UMLStereotype[MagicDrawUML]],
    umlR: OTIMOFMetamodelResourceExtent )
  : Vector[java.lang.Throwable] \&/ Vector[tables.profile.OTIMOFStereotype2ExtendedMetaclass]
  = ss.aggregate[\&/[Vector[java.lang.Throwable], Vector[tables.profile.OTIMOFStereotype2ExtendedMetaclass]]](\&/.That(Vector()))(
    {
      case (acc1, s) =>
        s.baseMetaProperties.aggregate(acc1)(
          {
            case (acc2, baseP) =>
              import Utils.VectorSemigroup
              acc2 append getStereotypeBaseProperty2ExtendedMetaclass(profileIRI, s, baseP, umlR)
          },
          _ append _)
    },
    _ append _)

  def getStereotypeBaseProperty2ExtendedMetaclass
  ( profileIRI: common.ResourceIRI,
    s: UMLStereotype[MagicDrawUML],
    baseP: UMLProperty[MagicDrawUML],
    umlR: OTIMOFMetamodelResourceExtent )
  : Vector[java.lang.Throwable] \&/ Vector[tables.profile.OTIMOFStereotype2ExtendedMetaclass]
  = baseP._type match {
    case None =>
      \&/.This(Vector(
        UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
          s"Stereotype base property ${baseP.qualifiedName.get} should be typed by a metaclass",
          Iterable(baseP))))
    case Some(extMC) =>
      import Utils.selectable

      umlR
        .classifiers
        .select { case umlMC: metamodel.MetaClass => umlMC }
        .find { umlMC => umlMC.name.value == extMC.name.get } match {
        case None =>
          \&/.This(Vector(
            UMLError.illegalElementError[MagicDrawUML, UMLProperty[MagicDrawUML]](
              s"Stereotype base property ${baseP.qualifiedName.get} refers to an unknown metaclass ${extMC.qualifiedName.get}",
              Iterable(baseP))))
        case Some(umlMC) =>
          \&/.That(Vector(
            tables.profile.OTIMOFStereotype2ExtendedMetaclass(
              resource = profileIRI,
              extendingStereotype = s.toOTIMOFEntityUUID,
              extendedMetaclass = umlMC.uuid)))
      }
  }

}