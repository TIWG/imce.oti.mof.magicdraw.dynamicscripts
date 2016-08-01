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
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Element
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.eclipse.emf.ecore.EReference
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUMLElement
import org.omg.oti.mof.schema._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.Int
import scala.Predef.String

object ModelLinkExporter {

  def toOrderedLinks
  (resource: common.ResourceIRI,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Iterable[tables.model.OTIMOFModelOrderedLink]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val compositeLinks
    : Iterable[tables.model.OTIMOFModelOrderedLink]
    = cache
      .metaclassContainmentAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[tables.model.OTIMOFModelOrderedLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              refs.toList.zipWithIndex.map { case (r: Element, i: Int) =>
                tables.model.OTIMOFModelOrderedLink(
                  resource,
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid,
                  index = i)
              }
            else
              Iterable.empty
          case _ =>
            Iterable.empty
        }

        links
      }

    val referenceLinks
    : Iterable[tables.model.OTIMOFModelOrderedLink]
    = cache
      .metaclassReferenceAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[tables.model.OTIMOFModelOrderedLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              refs.toList.zipWithIndex.map { case (r: Element, i: Int) =>
                tables.model.OTIMOFModelOrderedLink(
                  resource,
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid,
                  index = i)
              }
            else
              Iterable.empty
          case _ =>
            Iterable.empty
        }

        links
      }

    compositeLinks ++ referenceLinks
  }

  def toUnorderedLinks
  (resource: common.ResourceIRI,
   odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Iterable[tables.model.OTIMOFModelUnorderedLink]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val compositeLinks
    : Iterable[tables.model.OTIMOFModelUnorderedLink]
    = cache
      .metaclassContainmentAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[tables.model.OTIMOFModelUnorderedLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              Iterable.empty
            else
              refs.toList.map { case r: Element =>
                tables.model.OTIMOFModelUnorderedLink(
                  resource,
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid)
              }
          case _ =>
            Iterable.empty
        }

        links
      }

    val referenceLinks
    : Iterable[tables.model.OTIMOFModelUnorderedLink]
    = cache
      .metaclassReferenceAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[tables.model.OTIMOFModelUnorderedLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              Iterable.empty
            else
              refs.toList.map { case r: Element =>
                tables.model.OTIMOFModelUnorderedLink(
                  resource,
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid)
              }
          case _ =>
            Iterable.empty
        }

        links
      }

    compositeLinks ++ referenceLinks
  }

}