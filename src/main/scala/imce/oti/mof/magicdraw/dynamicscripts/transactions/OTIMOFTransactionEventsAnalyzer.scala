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

import com.nomagic.magicdraw.core.Application
import com.nomagic.magicdraw.uml.UUIDRegistry
import com.nomagic.uml2.ext.jmi.{ChangeElementPositionPropertyChangeEvent, IndexedPropertyChangeEvent, UML2MetamodelConstants}
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{Element, EnumerationLiteral}
import org.omg.oti.mof._

import scala.collection.immutable._
import scala.{None, Some, StringContext, Tuple2, Tuple4, Unit}
import scala.Predef.{String, require}

object OTIMOFTransactionEventsAnalyzer {

  val ignoreMDPropertyNames
  : Set[String]
  = Set(
    UML2MetamodelConstants.ID, // Given that the 'ID' change seems to always precede an 'INSTANCE_CREATED' change, we can ignore the 1st change
    "APPLIED_STEREOTYPES", "APPLIED_PROFILES", "TO_DO", "Active Hyperlink", "TYPE_MODIFIER",
    "allocatedFrom", "allocatedTo",
    "tracedFrom",
    "refines", "satisfies",
    "allSpecifyingElements", "specifyingElement",
    "verifies",
    "allRealizingElements", "realizingElement",
    "Base Classifiers",
    "allGeneralClassifiers",
    "allSpecificClassifiers", "specificClassifier",
    "realizingClassifier",
    "participatesInActivity", "participatesInInteraction",
    "describingBehavior",
    "realizingClass",
    "realizingComponent",
    "Realized Interfaces",
    "specifyingComponent",
    "specifyingUseCase",
    "describedUseCase",
    "metaclass", // this seems to correspond to Stereotype::/metaclass
    "multiplicity"
  )


  val SHOW_INV = false
}

class OTIMOFTransactionEventsAnalyzer
( val cache: MetamodelTransactionPropertyNameCache,
  val events: List[PropertyChangeEvent],
  val eventNotifiers: Seq[(schema.events.ModelChangedEvent, Iterable[String]) => Unit] )
  extends java.lang.Runnable {

  val app = Application.getInstance()
  val guiLog = app.getGUILog

  def notify
  ( changedEvent: schema.events.ModelChangedEvent,
    messages: Iterable[String]=Iterable.empty[String] )
  : Unit
  = eventNotifiers.foreach (_(changedEvent, messages))

  def run()
  : Unit
  = analyze(events)

  def analyze
  (events: List[PropertyChangeEvent])
  : Unit
  = events match {
    case event :: rest =>

      event.getSource match {

        case e: Element if !OTIMOFTransactionEventsAnalyzer.ignoreMDPropertyNames.contains(event.getPropertyName) =>

          val prop: String = event.getPropertyName
          val oldV: java.lang.Object = event.getOldValue
          val newV: java.lang.Object  = event.getNewValue
          val desc: String = e.toString + " : " + prop + " => Old value: " + oldV + ", new value: " + newV

          guiLog.log("\n")
          prop match {
            case UML2MetamodelConstants.INSTANCE_CREATED =>
              cache.otiMetaclasses.get(e.eClass) match {
                case None =>
                  guiLog.log(desc)
                  guiLog.log(s"NEW.??? ID=${e.getID}, UUID=${UUIDRegistry.getUUID(e)}")

                case Some(otiMC) =>
                  val changed = new schema.events.ModelElementChangedEvent(
                    changeKind = schema.events.AddedChange,
                    element = schema.tables.model.OTIMOFModelElement(
                      resource = schema.common.ResourceIRI(value = ""),
                      uuid = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                      metaClass = otiMC.uuid))
                  notify(changed, Iterable(desc))
              }
              analyze(rest)

            case UML2MetamodelConstants.BEFORE_DELETE =>
              guiLog.log(desc)
              guiLog.log(s"pre(DELETE) ID=${e.getID}, UUID=${UUIDRegistry.getUUID(e)}")
              analyze(rest)

            case UML2MetamodelConstants.INSTANCE_DELETED =>
              cache.otiMetaclasses.get(e.eClass) match {
                case None =>
                  guiLog.log(desc)
                  guiLog.log(s"NEW.??? ID=${e.getID}, UUID=${UUIDRegistry.getUUID(e)}")

                case Some(otiMC) =>
                  val changed = new schema.events.ModelElementChangedEvent(
                    changeKind = schema.events.DeletedChange,
                    element = schema.tables.model.OTIMOFModelElement(
                      resource = schema.common.ResourceIRI(value = ""),
                      uuid = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                      metaClass = otiMC.uuid))
                  notify(changed, Iterable(desc, s"post(DELETE) ID=${e.getID}, UUID=${UUIDRegistry.getUUID(e)}"))
              }
              analyze(rest)

            case _ =>
              cache.lookupPropertyChangeEvent(event) match {
                case None =>

                  cache.lookupInversePropertyChangeEvent(event) match {
                    case None =>

                      cache.metaclassOrderedAtomicAttributes.get(e.eClass) match {
                        case None =>
                          guiLog.log(desc)

                        case Some(name2attrib) =>
                          name2attrib.get(prop) match {
                            case None =>
                              guiLog.log(desc)
                              guiLog.log(s"ATTRIB: ???")

                            case Some(Tuple2(attrib, eAttrib)) =>

                              val changeKind =
                                if (null == oldV && null != newV)
                                  schema.events.AddedChange
                                else if (null != oldV && null == newV)
                                  schema.events.DeletedChange
                                else
                                  schema.events.ModifiedChange

                              event match {

                                case pevent: ChangeElementPositionPropertyChangeEvent =>

                                  if (eAttrib.isOrdered) {

                                    val changed = new schema.events.ModelOrderedAttributeAtomicValueChangedEvent(
                                      changeKind,
                                      attributeValue = schema.tables.values.OTIMOFOrderedAttributeAtomicValue(
                                        resource = schema.common.ResourceIRI(value = ""),
                                        entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                        attribute = attrib.uuid,
                                        value =
                                            if (changeKind == schema.events.DeletedChange)
                                              schema.common.AtomicValueRepresentation(oldV.toString)
                                            else
                                              schema.common.AtomicValueRepresentation(newV.toString),
                                        index = pevent.getIndex)
                                    )
                                    notify(changed, Iterable(desc))

                                  }

                                case ievent: IndexedPropertyChangeEvent =>

                                  if (eAttrib.isOrdered) {

                                    val changed = new schema.events.ModelOrderedAttributeAtomicValueChangedEvent(
                                      changeKind,
                                      attributeValue = schema.tables.values.OTIMOFOrderedAttributeAtomicValue(
                                        resource = schema.common.ResourceIRI(value = ""),
                                        entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                        attribute = attrib.uuid,
                                        value =
                                          if (changeKind == schema.events.DeletedChange)
                                            schema.common.AtomicValueRepresentation(oldV.toString)
                                          else
                                            schema.common.AtomicValueRepresentation(newV.toString),
                                        index = ievent.getIndex)
                                    )
                                    notify(changed, Iterable(desc))

                                  }

                                case _ =>
                                  ()
                              }
                          }
                      }

                      cache.metaclassOrderedEnumerationAttributes.get(e.eClass) match {
                        case None =>
                          guiLog.log(desc)

                        case Some(name2attrib) =>
                          name2attrib.get(prop) match {
                            case None =>
                              guiLog.log(desc)
                              guiLog.log(s"ATTRIB: ???")

                            case Some(Tuple2(attrib, eAttrib)) =>

                              val changeKind =
                                if (null == oldV && null != newV)
                                  schema.events.AddedChange
                                else if (null != oldV && null == newV)
                                  schema.events.DeletedChange
                                else
                                  schema.events.ModifiedChange

                              val value
                              : java.lang.Object
                              = if (changeKind == schema.events.DeletedChange)
                                oldV
                              else
                                newV

                              event match {

                                case pevent: ChangeElementPositionPropertyChangeEvent =>

                                  value match {
                                    case lit: EnumerationLiteral =>

                                      if (eAttrib.isOrdered) {

                                        val changed = new schema.events.ModelOrderedAttributeEnumerationLiteralValueChangedEvent(
                                          changeKind,
                                          attributeValue = schema.tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue(
                                            resource = schema.common.ResourceIRI(value = ""),
                                            entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                            attribute = attrib.uuid,
                                            value = schema.common.EntityUUID(UUIDRegistry.getUUID(lit)),
                                            index = pevent.getIndex)
                                        )
                                        notify(changed, Iterable(desc))

                                      }

                                    case _ =>
                                      ()
                                  }

                                case ievent: IndexedPropertyChangeEvent =>

                                  value match {
                                    case lit: EnumerationLiteral =>

                                      if (eAttrib.isOrdered) {

                                        val changed = new schema.events.ModelOrderedAttributeEnumerationLiteralValueChangedEvent(
                                          changeKind,
                                          attributeValue = schema.tables.values.OTIMOFOrderedAttributeEnumerationLiteralValue(
                                            resource = schema.common.ResourceIRI(value = ""),
                                            entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                            attribute = attrib.uuid,
                                            value = schema.common.EntityUUID(UUIDRegistry.getUUID(lit)),
                                            index = ievent.getIndex)
                                        )
                                        notify(changed, Iterable(desc))

                                      }

                                    case _ =>
                                      ()
                                  }

                                case _ =>
                                  ()
                              }
                          }
                      }

                      cache.metaclassUnorderedAtomicAttributes.get(e.eClass) match {
                        case None =>
                          guiLog.log(desc)

                        case Some(name2attrib) =>
                          name2attrib.get(prop) match {
                            case None =>
                              guiLog.log(desc)
                              guiLog.log(s"ATTRIB: ???")

                            case Some(Tuple2(attrib, eAttrib)) =>

                              val changeKind =
                                if (null == oldV && null != newV)
                                  schema.events.AddedChange
                                else if (null != oldV && null == newV)
                                  schema.events.DeletedChange
                                else
                                  schema.events.ModifiedChange

                              event match {

                                case pevent: ChangeElementPositionPropertyChangeEvent =>

                                  if (!eAttrib.isOrdered) {

                                    val changed = new schema.events.ModelUnorderedAttributeAtomicValueChangedEvent(
                                      changeKind,
                                      attributeValue = schema.tables.values.OTIMOFUnorderedAttributeAtomicValue(
                                        resource = schema.common.ResourceIRI(value = ""),
                                        entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                        attribute = attrib.uuid,
                                        value =
                                          if (changeKind == schema.events.DeletedChange)
                                            schema.common.AtomicValueRepresentation(oldV.toString)
                                          else
                                            schema.common.AtomicValueRepresentation(newV.toString))
                                    )
                                    notify(changed, Iterable(desc))

                                  }

                                case ievent: IndexedPropertyChangeEvent =>

                                  if (!eAttrib.isOrdered) {

                                    val changed = new schema.events.ModelUnorderedAttributeAtomicValueChangedEvent(
                                      changeKind,
                                      attributeValue = schema.tables.values.OTIMOFUnorderedAttributeAtomicValue(
                                        resource = schema.common.ResourceIRI(value = ""),
                                        entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                        attribute = attrib.uuid,
                                        value =
                                          if (changeKind == schema.events.DeletedChange)
                                            schema.common.AtomicValueRepresentation(oldV.toString)
                                          else
                                            schema.common.AtomicValueRepresentation(newV.toString))
                                    )
                                    notify(changed, Iterable(desc))

                                  }

                                case _ =>
                                  ()
                              }
                          }
                      }

                      cache.metaclassUnorderedEnumerationAttributes.get(e.eClass) match {
                        case None =>
                          guiLog.log(desc)

                        case Some(name2attrib) =>
                          name2attrib.get(prop) match {
                            case None =>
                              guiLog.log(desc)
                              guiLog.log(s"ATTRIB: ???")

                            case Some(Tuple2(attrib, eAttrib)) =>

                              val changeKind =
                                if (null == oldV && null != newV)
                                  schema.events.AddedChange
                                else if (null != oldV && null == newV)
                                  schema.events.DeletedChange
                                else
                                  schema.events.ModifiedChange

                              val value
                              : java.lang.Object
                              = if (changeKind == schema.events.DeletedChange)
                                oldV
                              else
                                newV

                              event match {

                                case pevent: ChangeElementPositionPropertyChangeEvent =>

                                  value match {
                                    case lit: EnumerationLiteral =>

                                      if (!eAttrib.isOrdered) {

                                        val changed = new schema.events.ModelUnorderedAttributeEnumerationLiteralValueChangedEvent(
                                          changeKind,
                                          attributeValue = schema.tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue(
                                            resource = schema.common.ResourceIRI(value = ""),
                                            entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                            attribute = attrib.uuid,
                                            value = schema.common.EntityUUID(UUIDRegistry.getUUID(lit)))
                                        )
                                        notify(changed, Iterable(desc))

                                      }
                                    case _ =>
                                      ()
                                  }

                                case ievent: IndexedPropertyChangeEvent =>

                                  value match {
                                    case lit: EnumerationLiteral =>

                                      if (!eAttrib.isOrdered) {

                                        val changed = new schema.events.ModelUnorderedAttributeEnumerationLiteralValueChangedEvent(
                                          changeKind,
                                          attributeValue = schema.tables.values.OTIMOFUnorderedAttributeEnumerationLiteralValue(
                                            resource = schema.common.ResourceIRI(value = ""),
                                            entity = schema.common.EntityUUID(UUIDRegistry.getUUID(e)),
                                            attribute = attrib.uuid,
                                            value = schema.common.EntityUUID(UUIDRegistry.getUUID(lit)))
                                        )
                                        notify(changed, Iterable(desc))

                                      }
                                    case _ =>
                                      ()
                                  }

                                case _ =>
                                  ()
                              }
                          }
                      }

                      analyze(rest)

                    // @todo Check for assoc.isOrdered & assoc.targetIsComposite
                    case Some(Tuple4(eClass, assoc, aFrom, aTo)) =>
                      require(aFrom.getEType.isInstance(e))
                      if (null == oldV && aTo.getEType.isInstance(newV)) {
                        if (OTIMOFTransactionEventsAnalyzer.SHOW_INV) {
                          guiLog.log(desc)
                          guiLog.log(s"INV.new eClass=${eClass.getName} => " +
                            s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                            s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                        }
                      } else if (aTo.getEType.isInstance(oldV) && null == newV) {
                        if (OTIMOFTransactionEventsAnalyzer.SHOW_INV) {
                          guiLog.log(desc)
                          guiLog.log(s"INV.del eClass=${eClass.getName} => " +
                            s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                            s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                        }
                      } else {
                        if (OTIMOFTransactionEventsAnalyzer.SHOW_INV) {
                          guiLog.log(desc)
                          guiLog.log(s"INV.??? eClass=${eClass.getName} => " +
                            s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                            s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                        }
                      }

                      analyze(rest)
                  }

                // @todo Check for assoc.isOrdered & assoc.targetIsComposite
                case Some(Tuple4(eClass, assoc, aFrom, aTo)) =>
                  require(aTo.getEType.isInstance(e))
                  if (null == oldV && aFrom.getEType.isInstance(newV)) {
                    guiLog.log(desc)
                    guiLog.log(s"FWD.new eClass=${eClass.getName} => " +
                      s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                      s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                  } else if (aFrom.getEType.isInstance(oldV) && null == newV) {
                    guiLog.log(desc)
                    guiLog.log(s"FWD.del eClass=${eClass.getName} => " +
                      s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                      s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                  } else {
                    guiLog.log(desc)
                    guiLog.log(s"FWD.??? eClass=${eClass.getName} => " +
                      s"${assoc.source.metaclassType.name.value}::${aTo.getName} -> " +
                      s"${aFrom.getName}::${assoc.target.metaclassType.name.value}")
                  }

                  analyze(rest)
              }
          }

        case _ =>
          analyze(rest)
      }

    case Nil =>
      ()
  }

}