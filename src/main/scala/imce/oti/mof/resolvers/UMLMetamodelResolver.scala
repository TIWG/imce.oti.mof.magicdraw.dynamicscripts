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

import org.omg.oti.mof.schema._
import imce.oti.mof.magicdraw.dynamicscripts.tiwg.Utils._

import scala.collection.immutable._
import scala.{Some,Tuple2,Tuple3}
import scala.Predef.{ArrowAssoc, String}

case class UMLMetamodelResolver
(primitiveTypesR
 : OTIMOFLibraryResourceExtent,

 umlR
 : OTIMOFMetamodelResourceExtent,

 metaclasses
 : Vector[metamodel.MetaClass],

 mc2DirectGeneralizations
 : Map[metamodel.MetaClass, Set[metamodel.MetaClass]],

 mc2DirectSpecializations
 : Map[metamodel.MetaClass, Set[metamodel.MetaClass]],

 mcName2UUID
 : Map[String, common.MetaClassUUID],

 mc2AllAttributes
 : Map[String, Set[features.DataTypedAttributeUnorderedProperty]])

object UMLMetamodelResolver {

  def initialize
  ( primitiveTypesR: OTIMOFLibraryResourceExtent,
    umlR: OTIMOFMetamodelResourceExtent )
  : UMLMetamodelResolver
  = {

    val metaclasses
    : Vector[metamodel.MetaClass]
    = umlR.classifiers.select { case mc: metamodel.MetaClass => mc }

    val mcGeneral2ParentPairs
    : Set[(metamodel.MetaClass, metamodel.MetaClass)]
    = umlR
      .generalizations
      .to[Set]
      .flatMap { g =>
        for {
          s <- metaclasses.find(_.uuid == g.specific)
          p <- metaclasses.find(_.uuid == g.general)
        } yield Tuple2(s, p)
      }

    val mc2DirectGeneralizations
    : Map[metamodel.MetaClass, Set[metamodel.MetaClass]]
    = mcGeneral2ParentPairs
      .groupBy(_._1)
      .map { case (sub, sub2sup) => sub -> sub2sup.map(_._2) }

    val mc2DirectSpecializations
    : Map[metamodel.MetaClass, Set[metamodel.MetaClass]]
    = mcGeneral2ParentPairs
      .groupBy(_._2)
      .map { case (sup, sub2sup) => sup -> sub2sup.map(_._1) }

    def getSpecializedMetaclasses
    ( mc: metamodel.MetaClass )
    : Set[metamodel.MetaClass]
    = mc2DirectSpecializations
      .getOrElse(mc, Set.empty[metamodel.MetaClass])

    val mcName2UUID
    : Map[String, common.MetaClassUUID]
    = metaclasses.map { mc =>
      mc.name.value -> mc.uuid
    }.toMap

    val mc2DirectAttributes
    : Map[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]]
    = umlR
      .metaclass2attribute
      .flatMap { mc2attrib =>
        for {
          mc <-
          metaclasses.find(_.uuid == mc2attrib.metaClass)

          attrib <-
          umlR.attributes.select { case a: features.DataTypedAttributeUnorderedProperty => a }.find(_.uuid == mc2attrib.attribute)
        } yield mc -> attrib
      }
      .groupBy(_._1)
      .map { case (mc, m2a) => mc -> m2a.map(_._2).to[Set] }


    val mc2AllAttributes
    : Map[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]]
    = metaclasses
      .to[Set]
      .foldLeft(Map.empty[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]]) {
        case (acc: Map[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]], mc: metamodel.MetaClass) =>

          val mcAttribs = mc2DirectAttributes.getOrElse(mc, Set.empty[features.DataTypedAttributeUnorderedProperty])

          def combine
          (current: Map[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]],
           s: metamodel.MetaClass)
          : Map[metamodel.MetaClass, Set[features.DataTypedAttributeUnorderedProperty]]
          = current.updated(s, mcAttribs ++ current.getOrElse(s, Set.empty[features.DataTypedAttributeUnorderedProperty]))

          transitiveClosure(mc, acc)(getSpecializedMetaclasses, combine)
      }

    UMLMetamodelResolver(primitiveTypesR, umlR,
      metaclasses,
      mc2DirectGeneralizations,
      mc2DirectSpecializations,
      mcName2UUID,
      mc2AllAttributes.map { case (s, as) => s.name.value -> as })
  }

  def transitiveClosure[E, V]
  ( initial: E,
    current: V )
  ( implicit next: E => Set[E], combine: (V, E) => V)
  : V
  = transitiveClosureLoop(Set[E](initial), Set.empty[E], current)

  @scala.annotation.tailrec
  def transitiveClosureLoop[E, V]
  ( queue: Set[E],
    visited: Set[E],
    acc: V)
  ( implicit next: E => Set[E], combine: (V, E) => V)
  : V
  = if (queue.isEmpty)
    acc
  else {
    val e = queue.head
    val rest = queue - e
    if (visited.contains(e))
      transitiveClosureLoop(rest, visited, acc)
    else
      transitiveClosureLoop(rest ++ next(e), visited + e, combine(acc, e))
  }


}