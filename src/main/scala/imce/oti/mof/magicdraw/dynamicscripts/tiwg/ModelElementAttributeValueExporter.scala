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
import java.lang.System

import com.nomagic.magicdraw.uml.UUIDRegistry
import com.nomagic.uml2.ext.jmi.helpers.StereotypesHelper
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{PrimitiveType, Element, ElementValue, InstanceValue}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Stereotype
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.omg.oti.magicdraw.uml.read.{MagicDrawUML, MagicDrawUMLElement}
import org.omg.oti.mof.schema._
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Int, None, Option, Some, StringContext, Tuple2, Tuple3, Tuple4}
import scala.Predef.ArrowAssoc
import scala.util.{Failure, Success, Try}
import scalaz._

object ModelElementAttributeValueExporter {

  import Utils.{IterableSemigroup,VectorSemigroup,selectable}

  def toModelElementAttributeValues
  (e: MagicDrawUMLElement,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
  = {
    val mdE = e.getMagicDrawElement
    val mdMC = mdE.eClass
    val attribs = cache.resolver.mc2AllAttributes.getOrElse(mdMC.getName, Set.empty[features.DataTypedAttributeProperty])

    mdMC.getEAllAttributes.aggregate[Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]](
      \&/.That(Vector())
    )(
      {
        case (acc, ea) if !ea.isDerived =>
          val name = ea.getName

          attribs
            .find { p =>
              name == p.name.value
            } match {

            case None =>
              acc

            case Some(attrib) =>
              if (!mdE.eIsSet(ea))
                acc
              else {
                // @todo it seems that MD UML Ecore metamodel does not have default values...
                val default = ea.getDefaultValue

                if (ea.isOrdered) {
                  val attributeValues = mdE.eGet(ea) match {
                    case vs: java.util.Collection[_] =>
                      vs
                        .zipWithIndex
                        .map { case (v, i) =>
                          model.ModelElementOrderedAttributeValue(
                            modelElement = e.toOTIMOFEntityUUID,
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = common.AtomicValueRepresentation(v.toString)),
                            index = i)
                        }
                        .toVector
                    case v =>
                      Vector(
                        model.ModelElementOrderedAttributeValue(
                          modelElement = e.toOTIMOFEntityUUID,
                          attributeValue = values.AtomicValue(
                            attribute = attrib.uuid,
                            value = common.AtomicValueRepresentation(v.toString)),
                          index = 0))
                  }
                  acc append \&/.That(attributeValues)
                } else {
                  val attributeValues = mdE.eGet(ea) match {
                    case vs: java.util.Collection[_] =>
                      vs
                        .map { v =>
                          model.ModelElementUnorderedAttributeValue(
                            modelElement = e.toOTIMOFEntityUUID,
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = common.AtomicValueRepresentation(v.toString)))
                        }
                        .toVector
                    case v =>
                      if (v != default)
                        Vector(
                          model.ModelElementUnorderedAttributeValue(
                            modelElement = e.toOTIMOFEntityUUID,
                            attributeValue = values.AtomicValue(
                              attribute = attrib.uuid,
                              value = common.AtomicValueRepresentation(v.toString))))
                      else
                        Vector()
                  }
                  acc append \&/.That(attributeValues)
                }
              }
          }

        case (acc, _) =>
          acc
      },
      _ append _)
  }


  def toModelElementStereotypeAttributeValues
  (e: MagicDrawUMLElement,
   otiCharacteristicsProvider: OTICharacteristicsProvider[MagicDrawUML])
  (implicit cache: MetamodelTransactionPropertyNameCache)
  : Vector[java.lang.Throwable] \&/ Vector[model.ModelElementAttributeValue]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))

    val tagValues
    : Iterable[model.ModelElementAttributeValue]
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
        : Iterable[model.ModelElementAttributeValue]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.map {
            case (v, i) =>
              model.ModelElementOrderedAttributeValue(
                modelElement = e.toOTIMOFEntityUUID,
                attributeValue = values.AtomicValue(
                  attribute = common.EntityUUID(UUIDRegistry.getUUID(f)),
                  value = common.AtomicValueRepresentation(v.toString)),
                index = i)
          }
        else
          slot.getValue.toList.map {
            case v =>
              model.ModelElementUnorderedAttributeValue(
                modelElement = e.toOTIMOFEntityUUID,
                attributeValue = values.AtomicValue(
                  attribute = common.EntityUUID(UUIDRegistry.getUUID(f)),
                  value = common.AtomicValueRepresentation(v.toString)))
          }
        tagValues
      }
    } yield tagValue

    \&/.That(tagValues.toVector)
  }
}