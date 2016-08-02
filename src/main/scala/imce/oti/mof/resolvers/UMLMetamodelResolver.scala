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
import scala.Tuple2
import scala.Predef.{ArrowAssoc, String}

case class UMLMetamodelResolver
(primitiveTypesR
 : OTIMOFLibraryTables,

 umlR
 : OTIMOFMetamodelTables,

 metaclasses
 : Vector[tables.metamodel.OTIMOFMetaClass],

 associations
 : Vector[tables.metamodel.OTIMOFMetaAssociation],

 associationViews
 : Vector[views.AssociationInfo],

 mc2DirectGeneralizations
 : Map[tables.metamodel.OTIMOFMetaClass, Set[tables.metamodel.OTIMOFMetaClass]],

 mc2DirectSpecializations
 : Map[tables.metamodel.OTIMOFMetaClass, Set[tables.metamodel.OTIMOFMetaClass]],

 mcName2UUID
 : Map[String, common.EntityUUID],

 mc2AllOrderedAtomicAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]],

 mc2AllOrderedEnumerationAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]],

 mc2AllOrderedStructuredAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]],

 mc2AllUnorderedAtomicAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]],

 mc2AllUnorderedEnumerationAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]],

 mc2AllUnorderedStructuredAttributes
 : Map[String, Set[features.DataTypedAttributeProperty]])

object UMLMetamodelResolver {

  def initialize
  ( primitiveTypesR: OTIMOFLibraryTables,
    umlR: OTIMOFMetamodelTables )
  : UMLMetamodelResolver
  = {

    val metaclasses
    : Vector[tables.metamodel.OTIMOFMetaClass]
    = umlR.metaClasses.to[Vector]

    val associations
    : Vector[tables.metamodel.OTIMOFMetaAssociation]
    = umlR.metaAssociations.to[Vector]

    val associationViews
    : Vector[views.AssociationInfo]
    = for {
      ma <- associations
      maUUID = ma.uuid.value
      ma2source <- umlR.metaAssociation2Source.find(_.association.value == maUUID)
      sourceUUID = ma2source.sourceEnd.value
      sourceEnd <- umlR.metaAssociationEnds.find(_.uuid.value == sourceUUID)
      sourceFind = (f: features.FeatureInfo) => f.feature.value == sourceUUID
      src2lower <- umlR.featureLowerBounds.find(sourceFind)
      src2upper <- umlR.featureUpperBounds.find(sourceFind)
      src2ord <- umlR.featureOrdering.find(sourceFind)
      src2Type <- umlR.metaAssociationEnd2MetaClass.find(sourceUUID == _.associationEnd.value)
      srcMetaclass <- metaclasses.find(src2Type.`type`.value == _.uuid.value)

      srcInfo = views.AssociationEndInfo(
        uuid = sourceEnd.uuid, name=sourceEnd.name,
        lower=src2lower.lower, upper=src2upper.upper,
        isOrdered=src2ord.isOrdered, metaclassType=srcMetaclass)

      ma2target <- umlR.metaAssociation2Target.find(_.association.value == maUUID)
      targetUUID = ma2target.targetEnd.value
      targetEnd <- umlR.metaAssociationEnds.find(_.uuid.value == targetUUID)
      targetFind = (f: features.FeatureInfo) => f.feature.value == targetUUID
      trg2lower <- umlR.featureLowerBounds.find(targetFind)
      trg2upper <- umlR.featureUpperBounds.find(targetFind)
      trg2ord <- umlR.featureOrdering.find(targetFind)
      trg2Type <- umlR.metaAssociationEnd2MetaClass.find(targetUUID == _.associationEnd.value)
      trgMetaclass <- metaclasses.find(trg2Type.`type`.value == _.uuid.value)

      trgInfo = views.AssociationEndInfo(
        uuid = targetEnd.uuid, name=targetEnd.name,
        lower=trg2lower.lower, upper=trg2upper.upper,
        isOrdered=trg2ord.isOrdered, metaclassType=trgMetaclass)

      info = views.AssociationInfo(
        uuid = ma.uuid,
        name = ma.name,
        source = srcInfo,
        target = trgInfo,
        targetIsComposite = targetEnd.isCompositeTarget)

      //_ = java.lang.System.out.println(info)
    } yield info

    val mcGeneral2ParentPairs
    : Set[(tables.metamodel.OTIMOFMetaClass, tables.metamodel.OTIMOFMetaClass)]
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
    : Map[tables.metamodel.OTIMOFMetaClass, Set[tables.metamodel.OTIMOFMetaClass]]
    = mcGeneral2ParentPairs
      .groupBy(_._1)
      .map { case (sub, sub2sup) => sub -> sub2sup.map(_._2) }

    val mc2DirectSpecializations
    : Map[tables.metamodel.OTIMOFMetaClass, Set[tables.metamodel.OTIMOFMetaClass]]
    = mcGeneral2ParentPairs
      .groupBy(_._2)
      .map { case (sup, sub2sup) => sup -> sub2sup.map(_._1) }

    def getSpecializedMetaclasses
    ( mc: tables.metamodel.OTIMOFMetaClass )
    : Set[tables.metamodel.OTIMOFMetaClass]
    = mc2DirectSpecializations
      .getOrElse(mc, Set.empty[tables.metamodel.OTIMOFMetaClass])

    def mc2AllAttributes
    (mc2DirectAttributes: Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]])
    : Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]]
    = metaclasses
      .to[Set]
      .foldLeft(Map.empty[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]]) {
        case (acc: Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]],
        mc: tables.metamodel.OTIMOFMetaClass) =>

          val mcAttribs = mc2DirectAttributes.getOrElse(mc, Set.empty[features.DataTypedAttributeProperty])

          def combine
          (current: Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]],
           s: tables.metamodel.OTIMOFMetaClass)
          : Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]]
          = current
            .updated(s,
              mcAttribs ++ current.getOrElse(s, Set.empty[features.DataTypedAttributeProperty]))

          transitiveClosure(mc, acc)(getSpecializedMetaclasses, combine)
      }

    def mc2Attributes
    (attributeMap: Iterable[tables.metamodel.OTIMOFMetaClass2Attribute])
    : Map[tables.metamodel.OTIMOFMetaClass, Set[features.DataTypedAttributeProperty]]
    = attributeMap
      .flatMap { mc2attrib =>
        for {

          mc <-
          metaclasses.find(_.uuid == mc2attrib.metaClass)

          attrib <-
          umlR.attributes
            .select { case a: features.DataTypedAttributeProperty => a }
            .find(_.uuid == mc2attrib.attribute)

        } yield mc -> attrib
      }
      .groupBy(_._1)
      .map { case (mc, m2a) => mc -> m2a.map(_._2).to[Set] }

    def mc2AllNamedAttributes
    (attributeMap: Iterable[tables.metamodel.OTIMOFMetaClass2Attribute])
    : Map[String, Set[features.DataTypedAttributeProperty]]
    = mc2AllAttributes(mc2Attributes(attributeMap)).map { case (s, as) => s.name.value -> as }

    val mcName2UUID
    : Map[String, common.EntityUUID]
    = metaclasses.map { mc =>
      mc.name.value -> mc.uuid
    }.toMap

    UMLMetamodelResolver(primitiveTypesR, umlR,
      metaclasses,
      associations,
      associationViews,
      mc2DirectGeneralizations,
      mc2DirectSpecializations,
      mcName2UUID,
      mc2AllOrderedAtomicAttributes = mc2AllNamedAttributes(umlR.metaClass2orderedAtomicAttribute),
      mc2AllOrderedEnumerationAttributes = mc2AllNamedAttributes(umlR.metaClass2orderedEnumerationAttribute),
      mc2AllOrderedStructuredAttributes = mc2AllNamedAttributes(umlR.metaClass2orderedStructuredAttribute),
      mc2AllUnorderedAtomicAttributes = mc2AllNamedAttributes(umlR.metaClass2unorderedAtomicAttribute),
      mc2AllUnorderedEnumerationAttributes = mc2AllNamedAttributes(umlR.metaClass2unorderedEnumerationAttribute),
      mc2AllUnorderedStructuredAttributes = mc2AllNamedAttributes(umlR.metaClass2unorderedStructuredAttribute))
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