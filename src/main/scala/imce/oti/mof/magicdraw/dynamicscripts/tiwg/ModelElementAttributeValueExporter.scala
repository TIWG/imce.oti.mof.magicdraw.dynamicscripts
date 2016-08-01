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

import com.nomagic.magicdraw.uml.UUIDRegistry
import com.nomagic.uml2.ext.jmi.helpers.StereotypesHelper
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{Enumeration,EnumerationLiteral,InstanceValue,PrimitiveType}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Stereotype
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{None, Option, Some, StringContext}
import scalaz._

object ModelElementAttributeValueExporter {

  import Utils.VectorSemigroup

  def toOrderedAtomicValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeAtomicValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllOrderedAtomicAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeAtomicValue]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .zipWithIndex
                    .map { case (v, i) =>
                      tables.values.OTIMOFOrderedAttributeAtomicValue(
                        resource,
                        entity = e.toOTIMOFEntityUUID,
                        attribute = attrib.uuid,
                        value = common.AtomicValueRepresentation(v.toString),
                        index = i)
                    }
                    .toVector
                case v =>
                  Vector(
                    tables.values.OTIMOFOrderedAttributeAtomicValue(
                    resource,
                    entity = e.toOTIMOFEntityUUID,
                    attribute = attrib.uuid,
                    value = common.AtomicValueRepresentation(v.toString),
                      index = 0))
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toOrderedEnumerationValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllOrderedEnumerationAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues
              : Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
              = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .zipWithIndex
                    .flatMap {
                      case (v: EnumerationLiteral, i) =>
                        Some(tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue(
                          resource,
                          entity = e.toOTIMOFEntityUUID,
                          attribute = attrib.uuid,
                          value = common.EntityUUID(UUIDRegistry.getUUID(v)),
                          index = i))
                      case _ =>
                        Option.empty[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
                    }
                    .toVector
                case v: EnumerationLiteral =>
                  Vector(
                    tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue(
                      resource,
                      entity = e.toOTIMOFEntityUUID,
                      attribute = attrib.uuid,
                      value = common.EntityUUID(UUIDRegistry.getUUID(v)),
                      index = 0))
                case _ =>
                  Vector.empty[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toOrderedStructuredValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeStructuredValueLink]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllOrderedStructuredAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeStructuredValueLink]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues
              : Vector[tables.values.OTIMOFOrderedAttributeStructuredValueLink]
              = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .zipWithIndex
                    .flatMap {
                      case (v: InstanceValue, i) =>
                        for {
                          is <- Option.apply(v.getInstance)
                        } yield
                          tables.values.OTIMOFOrderedAttributeStructuredValueLink(
                            resource,
                            entity = e.toOTIMOFEntityUUID,
                            attribute = attrib.uuid,
                            value = common.EntityUUID(UUIDRegistry.getUUID(is)),
                            index = i)
                      case _ =>
                        Option.empty[tables.values.OTIMOFOrderedAttributeStructuredValueLink]
                    }
                    .toVector
                case v: InstanceValue =>
                  Vector(
                    tables.values.OTIMOFOrderedAttributeStructuredValueLink(
                      resource,
                      entity = e.toOTIMOFEntityUUID,
                      attribute = attrib.uuid,
                      value = common.EntityUUID(UUIDRegistry.getUUID(v)),
                      index = 0))
                case _ =>
                  Vector.empty[tables.values.OTIMOFOrderedAttributeStructuredValueLink]
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toUnorderedAtomicValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeAtomicValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllUnorderedAtomicAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeAtomicValue]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .map { v =>
                      tables.values.OTIMOFUnorderedAttributeAtomicValue(
                        resource,
                        entity = e.toOTIMOFEntityUUID,
                        attribute = attrib.uuid,
                        value = common.AtomicValueRepresentation(v.toString))
                    }
                    .toVector
                case v =>
                  Vector(
                    tables.values.OTIMOFUnorderedAttributeAtomicValue(
                      resource,
                      entity = e.toOTIMOFEntityUUID,
                      attribute = attrib.uuid,
                      value = common.AtomicValueRepresentation(v.toString)))
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toUnorderedEnumerationValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllUnorderedEnumerationAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues
              : Vector[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]
              = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .flatMap {
                      case v: EnumerationLiteral =>
                        Some(tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue(
                          resource,
                          entity = e.toOTIMOFEntityUUID,
                          attribute = attrib.uuid,
                          value = common.EntityUUID(UUIDRegistry.getUUID(v))))
                      case _ =>
                        Option.empty[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]
                    }
                    .toVector
                case v: EnumerationLiteral =>
                  Vector(
                    tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue(
                      resource,
                      entity = e.toOTIMOFEntityUUID,
                      attribute = attrib.uuid,
                      value = common.EntityUUID(UUIDRegistry.getUUID(v))))
                case _ =>
                  Vector.empty[tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue]
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toUnorderedStructuredValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeStructuredValueLink]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllUnorderedStructuredAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC
      .getEAllAttributes
      .aggregate[Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFUnorderedAttributeStructuredValueLink]](
      \&/.That(Vector.empty)
    )(
      {
        case (acc, ea) if ea.isOrdered && !ea.isDerived && mdE.eIsSet(ea) =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>

              val attributeValues
              : Vector[tables.values.OTIMOFUnorderedAttributeStructuredValueLink]
              = mdE.eGet(ea) match {
                case vs: java.util.Collection[_] =>
                  vs
                    .flatMap {
                      case v: InstanceValue =>
                        for {
                          is <- Option.apply(v.getInstance)
                        } yield
                          tables.values.OTIMOFUnorderedAttributeStructuredValueLink(
                            resource,
                            entity = e.toOTIMOFEntityUUID,
                            attribute = attrib.uuid,
                            value = common.EntityUUID(UUIDRegistry.getUUID(is)))
                      case _ =>
                        Option.empty[tables.values.OTIMOFUnorderedAttributeStructuredValueLink]
                    }
                    .toVector
                case v: InstanceValue =>
                  Vector(
                    tables.values.OTIMOFUnorderedAttributeStructuredValueLink(
                      resource,
                      entity = e.toOTIMOFEntityUUID,
                      attribute = attrib.uuid,
                      value = common.EntityUUID(UUIDRegistry.getUUID(v))))
                case _ =>
                  Vector.empty[tables.values.OTIMOFUnorderedAttributeStructuredValueLink]
              }
              acc append \&/.That(attributeValues)

          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }

  def toOrderedAtomicTagValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeAtomicValue]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))

    val tagValues
    : Iterable[tables.values.OTIMOFOrderedAttributeAtomicValue]
    = for {
      is <- Option.apply(mdE.getAppliedStereotypeInstance).to[Iterable]
      appliedStereotypes = is.getClassifier.flatMap {
        case s: Stereotype =>
          Some(s)
        case _ =>
          None
      }
      slot <- is.getSlot.to[Iterable]
      f <- Option.apply(slot.getDefiningFeature).to[Iterable]
      fdt <- f.getType match {
        case dt: PrimitiveType =>
          Iterable(dt)
        case _ =>
          Iterable.empty[PrimitiveType]
      }
      if appliedStereotypes
        .exists { s => f == StereotypesHelper.getPropertyByName(s, f.getName) }
      tagValue <- {
        val tagValues
        : Iterable[tables.values.OTIMOFOrderedAttributeAtomicValue]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.map {
            case (v, i) =>
              tables.values.OTIMOFOrderedAttributeAtomicValue(
                resource,
                entity = e.toOTIMOFEntityUUID,
                attribute = common.EntityUUID(UUIDRegistry.getUUID(f)),
                value = common.AtomicValueRepresentation(v.toString),
                index = i)
          }
        else
          Iterable.empty

        tagValues
      }
    } yield tagValue

    \&/.That(tagValues.toVector)
  }

  def toOrderedEnumerationLiteralTagValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))

    val tagValues
    : Iterable[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
    = for {
      is <- Option.apply(mdE.getAppliedStereotypeInstance).to[Iterable]
      appliedStereotypes = is.getClassifier.flatMap {
        case s: Stereotype =>
          Some(s)
        case _ =>
          None
      }
      slot <- is.getSlot.to[Iterable]
      f <- Option.apply(slot.getDefiningFeature).to[Iterable]
      fdt <- f.getType match {
        case dt: Enumeration =>
          Iterable(dt)
        case _ =>
          Iterable.empty[Enumeration]
      }
      if appliedStereotypes
        .exists { s => f == StereotypesHelper.getPropertyByName(s, f.getName) }
      tagValue <- {
        val tagValues
        : Iterable[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.flatMap {
            case (v: EnumerationLiteral, i) =>
              Some(tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue(
                resource,
                entity = e.toOTIMOFEntityUUID,
                attribute = common.EntityUUID(UUIDRegistry.getUUID(f)),
                value = common.EntityUUID(UUIDRegistry.getUUID(v)),
                index = i))
            case _ =>
              Option.empty[tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue]
          }
        else
          Iterable.empty

        tagValues
      }
    } yield tagValue

    \&/.That(tagValues.toVector)
  }

  def toOrderedStructuredTagValues
  (resource: common.ResourceIRI,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (e: MagicDrawUMLElement)
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[tables.values.OTIMOFOrderedAttributeStructuredValueLink]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))

    val tagValues
    : Iterable[java.lang.Throwable]
    = for {
      is <- Option.apply(mdE.getAppliedStereotypeInstance).to[Iterable]
      appliedStereotypes = is.getClassifier.flatMap {
        case s: Stereotype =>
          Some(s)
        case _ =>
          None
      }
      slot <- is.getSlot.to[Iterable]
      f <- Option.apply(slot.getDefiningFeature).to[Iterable]
      fdt <- f.getType match {
        case _: Enumeration =>
          Iterable.empty[Enumeration]
        case _: PrimitiveType =>
          Iterable.empty[Enumeration]
        case dt =>
          Iterable(dt)
      }
      if appliedStereotypes
        .exists { s => f == StereotypesHelper.getPropertyByName(s, f.getName) }
      tagValue <- {
        val tagValues
        : Iterable[java.lang.Throwable]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.flatMap {
            case (v: InstanceValue, i) =>
              Some(new java.lang.IllegalArgumentException(
                s"Stereotype OTIMOFOrderedAttributeStructuredValueLink not yet supported for MagicDraw:\n"+
                s"resource=$resource\n"+
                s"entity=${mdE.getID}\n"+
                s"tag property=${f.getQualifiedName}\n"+
                s"value=${v.getInstance}\n"+
                s"index=$i"))
            case _ =>
              Option.empty[java.lang.Throwable]
          }
        else
          Iterable.empty

        tagValues
      }
    } yield tagValue

    \&/.This(tagValues.toVector)
  }
}