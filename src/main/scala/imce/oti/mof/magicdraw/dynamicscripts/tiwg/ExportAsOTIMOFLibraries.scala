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

import java.lang.System

import com.nomagic.magicdraw.core.Project
import org.omg.oti.json.uml.serialization.OTIJsonSerializationHelper
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLUtil}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.read.api.{UMLElement, UMLEnumeration, UMLPackage, UMLPrimitiveType}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{None, Option, Some, StringContext}
import scala.Predef.{ArrowAssoc, String}
import scalaz.Scalaz._
import scalaz._

/**
  * Export selected UML Packages as OTI MOF Libraries, that is, the exported OTIMOFLibraryResourceExtents
  * will only have the UML DataTypes defined in the selected packages.
  *
  * Everything outside the scope of an OTIMOFLibraryResourceExtent is ignored
  */
object ExportAsOTIMOFLibraries {

  def exportAsOTIMOFLibraryTables
  ( p: Project,
    odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
    d: Document[MagicDrawUML],
    pkg: UMLPackage[MagicDrawUML])
  : Vector[java.lang.Throwable] \&/ Vector[OTIMOFLibraryTables]
  = {
    val jHelper = OTIJsonSerializationHelper(odsa)
    implicit val ops = odsa.otiAdapter.umlOps

    val libIRI = d.toOTIMOFResourceIRI

    val lib = OTIMOFLibraryTables(
      resourceType = Iterable(tables.OTIMOFResourceType(resource=libIRI, kind=tables.OTIMOFResourceLibraryKind)),
      importedLibraries = pkg.packageImport.toVector.flatMap { pi =>
        pi.importedPackage match {
          case Some(ipkg: UMLPackage[MagicDrawUML]) =>
            odsa.ds.lookupDocumentByScope(ipkg) match {
              case Some(dipkg) =>
                Some(OTIMOFResourceLibraryImport(
                  importingResource = libIRI,
                  importedLibrary = dipkg.toOTIMOFResourceIRI))
              case _ =>
                System.out.println(
                  s"Profile `${pkg.qualifiedName.get}` imports " +
                    s"a package without a known OTI Document PackageURI: `${ipkg.qualifiedName.get}`")
                None
            }
          case _ =>
            None
        }
      },

      primitiveDataTypes = d.extent.flatMap(toPrimitiveDataType(libIRI)).toVector.sortBy(_.name),
      enumerationDataTypes = d.extent.flatMap(toEnumerationDataType(libIRI)).toVector.sortBy(_.name),
      enumeration2literals = d.extent.flatMap(toEnumeration2Literals(libIRI)).toVector,
      enumerationLiterals = d.extent.flatMap(toEnumerationLiterals(libIRI)).toVector,

      structuredDataTypes = Iterable.empty[tables.library.OTIMOFStructuredDataType],
      generalizations = Iterable.empty[tables.library.OTIMOFStructuredDataTypeGeneralization],
      structure2attribute = Iterable.empty[tables.library.OTIMOFStructuredDatatype2Attribute],

      attributes = Iterable.empty[features.DataTypedAttributeProperty],

      featureLowerBounds = Iterable.empty[features.FeatureLowerBound],
      featureUpperBounds = Iterable.empty[features.FeatureUpperBound],
      featureOrdering = Iterable.empty[features.FeatureOrdering],

      attribute2type = Iterable.empty[features.AttributeProperty2DataType]
    )

    \&/.That(Vector(lib))
  }

  val primitiveTypeMap
  : Map[String, common.DatatypeAbbrevIRI]
  = Map(
    "Boolean" -> common.DatatypeAbbrevIRI("xsd:boolean"),
    "Integer" -> common.DatatypeAbbrevIRI("xsd:integer"),
    "Real" -> common.DatatypeAbbrevIRI("xsd:double"),
    "String" -> common.DatatypeAbbrevIRI("xsd:string"),
    "UnlimitedNatural" -> common.DatatypeAbbrevIRI("xsd:string")
    )

  def toPrimitiveDataType
  (libIRI: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit ops: MagicDrawUMLUtil)
  : Option[tables.library.OTIMOFPrimitiveDataType]
  = e match {
    case pt: UMLPrimitiveType[MagicDrawUML] =>
  tables.library.OTIMOFPrimitiveDataType(
        resource = libIRI,
        uuid = pt.toOTIMOFEntityUUID,
        name = common.Name(pt.name.get),
        datatypeMapDefinition = primitiveTypeMap(pt.name.get)
      ).some
    case _ =>
      None
  }

  def toEnumerationDataType
  (libIRI: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit ops: MagicDrawUMLUtil)
  : Option[tables.library.OTIMOFEnumerationDataType]
  = e match {
    case pt: UMLEnumeration[MagicDrawUML] =>
      tables.library.OTIMOFEnumerationDataType(
        resource = libIRI,
        uuid = pt.toOTIMOFEntityUUID,
        name = common.Name(pt.name.get)
      ).some
    case _ =>
      None
  }

  def toEnumeration2Literals
  (libIRI: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit ops: MagicDrawUMLUtil)
  : Vector[tables.library.OTIMOFEnumeration2Literal]
  = e match {
    case en: UMLEnumeration[MagicDrawUML] =>
      val enUUID = en.toOTIMOFEntityUUID
      en.ownedLiteral.toVector.zipWithIndex.map { case (l, index) =>
        tables.library.OTIMOFEnumeration2Literal(
          resource = libIRI,
          enumeration = enUUID,
          literal = l.toOTIMOFEntityUUID,
          index)
      }
    case _ =>
      Vector.empty
  }

  def toEnumerationLiterals
  (libIRI: common.ResourceIRI)
  (e: UMLElement[MagicDrawUML])
  (implicit ops: MagicDrawUMLUtil)
  : Vector[common.EnumerationLiteralValue]
  = e match {
    case en: UMLEnumeration[MagicDrawUML] =>
      en.ownedLiteral.toVector.map { l =>
        common.EnumerationLiteralValue(
          resource = libIRI,
          literal = l.toOTIMOFEntityUUID,
          name = l.name.get)
      }
    case _ =>
      Vector.empty
  }

}