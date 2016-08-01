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
package imce.oti.mof.magicdraw.dynamicscripts.transactions

import java.beans.PropertyChangeEvent
import java.lang.System

import com.nomagic.uml2.ext.magicdraw.metadata.UMLPackage
import org.eclipse.emf.ecore._
import org.eclipse.emf.ecore.EClass
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EPackage
import org.eclipse.emf.ecore.EReference

import imce.oti.mof.resolvers.UMLMetamodelResolver
import org.omg.oti.mof.schema._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Int, None, Option, Ordering, Some, StringContext, Tuple2, Tuple3, Tuple4}
import scala.Predef.{ArrowAssoc, String, augmentString, require}

case class MetamodelTransactionPropertyNameCache
( resolver: UMLMetamodelResolver,

  otiMetaclasses: Map[EClass, metamodel.MetaClass],

  metaclasses: SortedSet[EClass],

  taxonomy: Map[EClass, Seq[EClass]],

  metaclassContainmentAssociationForwardProperties
  : Map[EClass, Map[String, Tuple3[views.AssociationInfo, EReference, EReference]]],

  metaclassReferenceAssociationForwardProperties
  : Map[EClass, Map[String, Tuple3[views.AssociationInfo, EReference, EReference]]],

  metaclassContainmentAssociationReverseProperties
  : Map[EClass, Map[String, Tuple3[views.AssociationInfo, EReference, EReference]]],

  metaclassReferenceAssociationReverseProperties
  : Map[EClass, Map[String, Tuple3[views.AssociationInfo, EReference, EReference]]],

  metaclassOrderedAtomicAttributes
  : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]],

  metaclassOrderedEnumerationAttributes
  : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]],

  metaclassUnorderedAtomicAttributes
  : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]],

  metaclassUnorderedEnumerationAttributes
  : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]]) {

  def lookupContainmentPropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = ev.getSource match {
    case e: EObject =>
      lookupContainment(e, ev.getPropertyName)
    case _ =>
      None
  }

  def lookupContainment(e: EObject, metaProperty: String)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = for {
    eMC <- Option.apply(e.eClass)
    tuples <- metaclassContainmentAssociationForwardProperties.get(eMC)
    tuple <- tuples.get( metaProperty )
    _ = require(tuple._3.getEType.isInstance(e))
  } yield Tuple4(eMC, tuple._1, tuple._2, tuple._3)

  def lookupReferencePropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = ev.getSource match {
    case e: EObject =>
      lookupReference(e, ev.getPropertyName)
    case _ =>
      None
  }

  def lookupReference(e: EObject, metaProperty: String)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = for {
    eMC <- Option.apply(e.eClass)
    tuples <- metaclassReferenceAssociationForwardProperties.get(eMC)
    tuple <- tuples.get( metaProperty )
    _ = require(tuple._3.getEType.isInstance(e))
  } yield Tuple4(eMC, tuple._1, tuple._2, tuple._3)

  def lookupPropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = lookupContainmentPropertyChangeEvent(ev) orElse lookupReferencePropertyChangeEvent(ev)

  def lookupInverseContainmentPropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = ev.getSource match {
    case e: EObject =>
      lookupInverseContainment(e, ev.getPropertyName)
    case _ =>
      None
  }

  def lookupInverseContainment(e: EObject, metaProperty: String)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = for {
    eMC <- Option.apply(e.eClass)
    tuples <- metaclassContainmentAssociationReverseProperties.get(eMC)
    tuple <- tuples.values.find ( metaProperty == _._3.getName )
    _ = require(tuple._2.getEType.isInstance(e))
  } yield Tuple4(eMC, tuple._1, tuple._2, tuple._3)

  def lookupInverseReferencePropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = ev.getSource match {
    case e: EObject =>
      lookupInverseReference(e, ev.getPropertyName)
    case _ =>
      None
  }

  def lookupInverseReference(e: EObject, metaProperty: String)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = for {
    eMC <- Option.apply(e.eClass)
    tuples <- metaclassReferenceAssociationReverseProperties.get(eMC)
    tuple <- tuples.values.find ( metaProperty == _._3.getName )
    _ = require(tuple._2.getEType.isInstance(e))
  } yield Tuple4(eMC, tuple._1, tuple._2, tuple._3)

  def lookupInversePropertyChangeEvent(ev: PropertyChangeEvent)
  : Option[(EClass, views.AssociationInfo, EReference, EReference)]
  = lookupInverseContainmentPropertyChangeEvent(ev) orElse lookupInverseReferencePropertyChangeEvent(ev)

}

object MetamodelTransactionPropertyNameCache {

  implicit var ECLASS_COMPARATOR
  : Ordering[EClass] = new Ordering[EClass]() {
    def compare(ec1: EClass, ec2: EClass): Int = {
      return ec1.getName.compareTo(ec2.getName)
    }
  }

  def apply
  (umlMM: UMLMetamodelResolver)
  : MetamodelTransactionPropertyNameCache
  = {

    val eUML: EPackage = UMLPackage.eINSTANCE

    val metaclasses
    : SortedSet[EClass]
    = eUML
      .getEClassifiers
      .flatMap {
        case mc: EClass =>
          Some(mc)
        case _ => None
      }
      .foldLeft(new TreeSet[EClass]()){ _ + _ }

    val otiMetaclasses
    : Map[EClass, metamodel.MetaClass]
    = metaclasses.flatMap { mc =>
      for {
        otiMC <- umlMM.metaclasses.find(_.name.value == mc.getName)
        if !mc.isAbstract
      } yield mc -> otiMC
    }.toMap

    val taxonomy
    : Map[EClass, Seq[EClass]]
    = metaclasses.map { mc =>
      mc -> (mc.getEAllSuperTypes.to[Seq] :+ mc).sortWith { case (c1: EClass, c2: EClass) =>
          c1.isSuperTypeOf(c2)
      }
    }.toMap

    val specializations
    : Map[EClass, Set[EClass]]
    = taxonomy.foldLeft(Map.empty[EClass, Set[EClass]]) { case (acc, (sub, sups)) =>
        sups.foldLeft(acc) { case (inc, sup) =>
            inc.updated(
              sup,
              inc.getOrElse(sup, Set[EClass](sup)) + sub)
        }
    }

    val metaclassOrderedAtomicAttributes
    : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]]
    = metaclasses
      .map { mc =>
        val tuples = for {
          attrib <- umlMM.mc2AllOrderedAtomicAttributes.getOrElse(mc.getName, Set.empty[features.DataTypedAttributeProperty])
          a <- mc.getEAllAttributes.find ( attrib.name.value == _.getName )
        } yield a.getName -> Tuple2(attrib, a)
        mc -> tuples.toMap
      }
      .toMap

    val metaclassOrderedEnumerationAttributes
    : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]]
    = metaclasses
      .map { mc =>
        val tuples = for {
          attrib <- umlMM.mc2AllOrderedEnumerationAttributes.getOrElse(mc.getName, Set.empty[features.DataTypedAttributeProperty])
          a <- mc.getEAllAttributes.find ( attrib.name.value == _.getName )
        } yield a.getName -> Tuple2(attrib, a)
        mc -> tuples.toMap
      }
      .toMap

    val metaclassUnorderedAtomicAttributes
    : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]]
    = metaclasses
      .map { mc =>
        val tuples = for {
          attrib <- umlMM.mc2AllUnorderedAtomicAttributes.getOrElse(mc.getName, Set.empty[features.DataTypedAttributeProperty])
          a <- mc.getEAllAttributes.find ( attrib.name.value == _.getName )
        } yield a.getName -> Tuple2(attrib, a)
        mc -> tuples.toMap
      }
      .toMap

    val metaclassUnorderedEnumerationAttributes
    : Map[EClass, Map[String, Tuple2[features.DataTypedAttributeProperty, EAttribute]]]
    = metaclasses
      .map { mc =>
        val tuples = for {
          attrib <- umlMM.mc2AllUnorderedEnumerationAttributes.getOrElse(mc.getName, Set.empty[features.DataTypedAttributeProperty])
          a <- mc.getEAllAttributes.find ( attrib.name.value == _.getName )
        } yield a.getName -> Tuple2(attrib, a)
        mc -> tuples.toMap
      }
      .toMap

    val metaclassContainmentAssociationForwardProperties
    : Map[EClass, Map[String, (views.AssociationInfo, EReference, EReference)]]
    = taxonomy
      .map { case (sup, parents) =>
        val tuples =
          parents.foldLeft(Map.empty[String, (views.AssociationInfo, EReference, EReference)]) { case (acc1, mc) =>
              mc.getEReferences.foldLeft(acc1) { case (acc2, p) =>
                Option.apply(p.getEOpposite) match {
                  case None =>
                    acc2

                  case Some(po) =>
                    if (p.isContainment && p.isChangeable && po.isContainer && po.isChangeable) {
                      val targetName = p.getName
                      val targetType = p.getEType.getName
                      val sourceName = po.getName
                      val sourceType = po.getEType.getName
                      umlMM.associationViews
                        .find { av =>
                          sourceName == av.source.name.value &&
                            sourceType == av.source.metaclassType.name.value &&
                            targetName == av.target.name.value &&
                            targetType == av.target.metaclassType.name.value
                        } match {
                        case None =>
                          acc2

                        case Some(view) =>
                          System.out.println(
                            "\n### (from %s): %s => container %s (opposite=%s)\n%s"
                              .format(
                                sup.getName, mc.getName, sourceName, targetName, view))

                          val key = p.getName

                          key.indexOf("__from_") match {
                            case -1 =>
                              acc2 + (key -> Tuple3(view, p, po))

                            case k =>
                              val redefined = key.substring(0, k)
                              System.out.println(s"### override $redefined:\n${acc2(redefined)._1}")
                              acc2.updated(redefined, Tuple3(view, p, po))
                          }
                      }
                    } else
                      acc2
                }
              }
          }
        sup -> tuples
      }

    val metaclassContainmentAssociationReverseProperties
    : Map[EClass, Set[(views.AssociationInfo, EReference, EReference)]]
    = metaclasses.foldLeft(Map.empty[EClass, Set[(views.AssociationInfo, EReference, EReference)]]) {
      case (acc1, mc) =>
        val tuples = for {
          p <- mc.getEReferences
          if p.isContainment && p.isChangeable
          po <- Option.apply(p.getEOpposite)
          if po.isContainer && po.isChangeable
          rc <- p.getEType match {
            case rt: EClass =>
              Some(rt)
            case _ =>
              None
          }
          targetName = p.getName
          targetType = rc.getName
          sourceName = po.getName
          sourceType = po.getEType.getName
          view <- umlMM.associationViews.find { av =>
            sourceName == av.source.name.value && sourceType == av.source.metaclassType.name.value &&
              targetName == av.target.name.value && targetType == av.target.metaclassType.name.value
          }
          _ = System.out.println(
            "\n### (rev %s): %s => container %s (opposite=%s)\n%s"
              .format(
                rc.getName, mc.getName, sourceName, targetName, view))
        } yield rc -> Tuple3(view, p, po)
        tuples.foldLeft(acc1) { case (acc2, (rc, tuple)) =>
          specializations
            .getOrElse(rc, Set[EClass](rc))
            .foldLeft(acc2) { case (acc3, sc) =>
              acc3.updated(
                sc,
                acc3.getOrElse(sc, Set.empty[(views.AssociationInfo, EReference, EReference)]) + tuple)
            }
        }
    }

    val metaclassReferenceAssociationForwardProperties
    : Map[EClass, Seq[(views.AssociationInfo, EReference, EReference)]]
    = taxonomy
      .map { case (sup, parents) =>
        val tuples = for {
          mc <- parents
          p <- mc.getEReferences
          if !p.isContainer && !p.isContainment && !p.isDerived
          po <- Option.apply(p.getEOpposite)
          if !po.isContainer && !po.isContainment && !po.isDerived
          targetName = p.getName
          targetType = p.getEType.getName
          sourceName = po.getName
          sourceType = po.getEType.getName
          view <- umlMM.associationViews.find{ av =>
            sourceName == av.source.name.value && sourceType == av.source.metaclassType.name.value &&
              targetName == av.target.name.value && targetType == av.target.metaclassType.name.value
          }
          _ =  System.out.println(
            "\n### (from %s): %s => reference %s -> %s\n%s"
              .format(
                sup.getName, mc.getName,
                sourceName, targetName,
                view))
        } yield Tuple3(view, p, po)
        sup -> tuples
      }

    val metaclassReferenceAssociationReverseProperties
    : Map[EClass, Set[(views.AssociationInfo, EReference, EReference)]]
    = metaclasses.foldLeft(Map.empty[EClass, Set[(views.AssociationInfo, EReference, EReference)]]) {
      case (acc1, mc) =>
        val tuples = for {
          p <- mc.getEReferences
          if !p.isContainer && !p.isContainment && !p.isDerived
          po <- Option.apply(p.getEOpposite)
          if !po.isContainer && !po.isContainment && !po.isDerived
          rc <- p.getEType match {
            case rt: EClass =>
              Some(rt)
            case _ =>
              None
          }
          targetName = p.getName
          targetType = rc.getName
          sourceName = po.getName
          sourceType = po.getEType.getName
          view <- umlMM.associationViews.find { av =>
            sourceName == av.source.name.value && sourceType == av.source.metaclassType.name.value &&
              targetName == av.target.name.value && targetType == av.target.metaclassType.name.value
          }
          _ = System.out.println(
            "\n### (rev %s): %s => reference %s -> %s\n%s"
              .format(
                targetType, mc.getName,
                sourceName, targetName,
                view))
        } yield rc -> Tuple3(view, p, po)
        tuples.foldLeft(acc1) { case (acc2, (rc, tuple)) =>
          specializations
            .getOrElse(rc, Set[EClass](rc))
            .foldLeft(acc2) { case (acc3, sc) =>
              acc3.updated(
                sc,
                acc3.getOrElse(sc, Set.empty[(views.AssociationInfo, EReference, EReference)]) + tuple)
            }
        }
    }

    val allCompositePairs =
      metaclassContainmentAssociationForwardProperties
        .values
        .foldLeft(Set.empty[(views.AssociationInfo, EReference, EReference)]) { case (acc, tupleMap) =>
            acc ++ tupleMap.values
        }

    val allReferencePairs =
      metaclassReferenceAssociationForwardProperties
        .values
        .foldLeft(Set.empty[(views.AssociationInfo, EReference, EReference)]){ _ ++ _}

    metaclassContainmentAssociationForwardProperties.foreach { case (eClass, tuples) =>
      System.out.println(s"\nforward containment associations for ${eClass.getName}")
      tuples.foreach { case (prop, (view, from, to)) =>
        System.out.println(
          s"${view.source.metaclassType.name.value}::${to.getName} -> " +
            s"${from.getName}::${view.target.metaclassType.name.value}")
      }
    }

    val cache =
    MetamodelTransactionPropertyNameCache(
      umlMM,
      otiMetaclasses,
      metaclasses,
      taxonomy,
      metaclassContainmentAssociationForwardProperties,
      metaclassReferenceAssociationForwardProperties.map { case (eClass, tuples) =>
        System.out.println(s"\nforward reference associations for ${eClass.getName}")
        eClass -> tuples.map { case (view, from, to) =>
          System.out.println(
            s"${view.source.metaclassType.name.value}::${to.getName} -> "+
              s"${from.getName}::${view.target.metaclassType.name.value}")
          from.getName -> Tuple3(view, from, to)
        }.toMap
      },
      metaclassContainmentAssociationReverseProperties.map { case (eClass, tuples) =>
        System.out.println(s"\nreverse containment associations for ${eClass.getName}")
        eClass -> tuples.map { case (view, from, to) =>
          System.out.println(
            s"${view.source.metaclassType.name.value}::${to.getName} -> "+
              s"${from.getName}::${view.target.metaclassType.name.value}")
          from.getName -> Tuple3(view, from, to)
        }.toMap
      },
      metaclassReferenceAssociationReverseProperties.map { case (eClass, tuples) =>
        System.out.println(s"\nreverse reference associations for ${eClass.getName}")
        eClass -> tuples.map { case (view, from, to) =>
          System.out.println(
            s"${view.source.metaclassType.name.value}::${to.getName} -> "+
              s"${from.getName}::${view.target.metaclassType.name.value}")
          from.getName -> Tuple3(view, from, to)
        }.toMap
      },
      metaclassOrderedAtomicAttributes,
      metaclassOrderedEnumerationAttributes,
      metaclassUnorderedAtomicAttributes,
      metaclassUnorderedEnumerationAttributes)

    System.out.println(s"\nMetamodel Composite association pairs: ${allCompositePairs.size}")
    System.out.println(s"Metamodel Reference association pairs: ${allReferencePairs.size}\n")

    cache
  }
}