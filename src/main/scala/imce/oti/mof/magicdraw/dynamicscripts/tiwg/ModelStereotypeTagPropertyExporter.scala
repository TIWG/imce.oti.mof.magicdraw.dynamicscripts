package imce.oti.mof.magicdraw.dynamicscripts.tiwg

import com.nomagic.magicdraw.uml.UUIDRegistry
import com.nomagic.uml2.ext.jmi.helpers.StereotypesHelper
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.{Class => MDClass, Element, ElementValue, InstanceValue}
import com.nomagic.uml2.ext.magicdraw.mdprofiles.Stereotype
import imce.oti.mof.magicdraw.dynamicscripts.transactions.MetamodelTransactionPropertyNameCache
import org.eclipse.emf.ecore.EReference
import org.omg.oti.magicdraw.uml.canonicalXMI.helper.MagicDrawOTIDocumentSetAdapterForDataProvider
import org.omg.oti.magicdraw.uml.read.MagicDrawUMLElement
import org.omg.oti.mof.schema._

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Int, None, Option, Some}

object ModelStereotypeTagPropertyExporter {

  def toTagValue
  (odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Vector[model.AppliedStereotypePropertyReference]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val tagValues
    : Iterable[model.AppliedStereotypePropertyReference]
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
        : Iterable[model.AppliedStereotypePropertyReference]
        = if (f.isOrdered)
          slot.getValue.toList.zipWithIndex.map {
            case (ev: ElementValue, i: Int) =>
              model.AppliedStereotypePropertyOrderedReference(
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(ev.getElement)),
                index = i)
            case (iv: InstanceValue, i: Int) =>
              model.AppliedStereotypePropertyOrderedReference(
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(iv.getInstance)),
                index = i)
          }
        else
          slot.getValue.toList.map {
            case (ev: ElementValue) =>
              model.AppliedStereotypePropertyUnorderedReference(
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(ev.getElement)))
            case (iv: InstanceValue) =>
              model.AppliedStereotypePropertyUnorderedReference(
                modelElement = sourceUUID,
                associationTargetEnd = common.EntityUUID(UUIDRegistry.getUUID(f)),
                referencedElement = common.EntityUUID(UUIDRegistry.getUUID(iv.getInstance)))
          }
        tagValues
      }
    } yield tagValue

    tagValues.toVector
  }

}
