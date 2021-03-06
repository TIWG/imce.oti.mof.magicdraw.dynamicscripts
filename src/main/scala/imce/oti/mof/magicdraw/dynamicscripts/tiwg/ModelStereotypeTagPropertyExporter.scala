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
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{Class => MDClass, ElementValue, InstanceValue}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Stereotype
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUMLElement
import org.omg.oti.mof.schema._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Int, None, Option, Some}

object ModelStereotypeTagPropertyExporter {

  def toOrderedReferences
  (resource: common.ResourceIRI,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Iterable[tables.model.OTIMOFAppliedStereotypePropertyOrderedReference]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val tagValues
    : Iterable[tables.model.OTIMOFAppliedStereotypePropertyOrderedReference]
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
      fmc <- f.getType match {
        case mc: MDClass =>
          Iterable(mc)
        case _ =>
          Iterable.empty[MDClass]
      }
      if appliedStereotypes
        .exists { s => f == StereotypesHelper.getPropertyByName(s, f.getName) }
      tagValue <- {
        val tagValues
        : Iterable[tables.model.OTIMOFAppliedStereotypePropertyOrderedReference]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.map {
            case (ev: ElementValue, i: Int) =>
              tables.model.OTIMOFAppliedStereotypePropertyOrderedReference(
                resource,
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(ev.getElement)),
                index = i)
            case (iv: InstanceValue, i: Int) =>
              tables.model.OTIMOFAppliedStereotypePropertyOrderedReference(
                resource,
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(iv.getInstance)),
                index = i)
          }
        else
          Iterable.empty
        tagValues
      }
    } yield tagValue

    tagValues
  }

  def toUnorderedReferences
  (resource: common.ResourceIRI,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Iterable[tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val tagValues
    : Iterable[tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference]
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
      fmc <- f.getType match {
        case mc: MDClass =>
          Iterable(mc)
        case _ =>
          Iterable.empty[MDClass]
      }
      if appliedStereotypes
        .exists { s => f == StereotypesHelper.getPropertyByName(s, f.getName) }
      tagValue <- {
        val tagValues
        : Iterable[tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference]
        = if (f.isOrdered)
          Iterable.empty
        else
          slot.getValue.toList.map {
            case (ev: ElementValue) =>
              tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference(
                resource,
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(ev.getElement)))
            case (iv: InstanceValue) =>
              tables.model.OTIMOFAppliedStereotypePropertyUnorderedReference(
                resource,
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(iv.getInstance)))
          }

        tagValues
      }
    } yield tagValue

    tagValues
  }

}