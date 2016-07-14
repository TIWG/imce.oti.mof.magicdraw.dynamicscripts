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

  def toModelLinks
  (odsa: MagicDrawOTIDocumentSetAdapterForDataProvider,
   cache: MetamodelTransactionPropertyNameCache)
  (e: MagicDrawUMLElement)
  : Vector[model.ModelLink]
  = {
    val mdE = e.getMagicDrawElement
    val sourceUUID = common.EntityUUID(UUIDRegistry.getUUID(mdE))
    val mdMC = mdE.eClass

    val compositeLinks
    : Iterable[model.ModelLink]
    = cache
      .metaclassContainmentAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[model.ModelLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              refs.toList.zipWithIndex.map { case (r: Element, i: Int) =>
                model.ModelOrderedLink(
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid,
                  index = i)
              }
            else
              refs.toList.map { case r: Element =>
                model.ModelUnorderedLink(
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid)
              }
          case _ =>
            Iterable.empty[model.ModelLink]
        }

        links
      }

    val referenceLinks
    : Iterable[model.ModelLink]
    = cache
      .metaclassReferenceAssociationForwardProperties
      .getOrElse(mdMC, Map.empty[String, (views.AssociationInfo, EReference, EReference)])
      .flatMap { case (propertyName, (assoc, sourceRef, targetRef)) =>

        val links
        : Iterable[model.ModelLink]
        = mdE.eGet(sourceRef) match {
          case refs: java.util.Collection[_] =>
            if (assoc.isOrdered)
              refs.toList.zipWithIndex.map { case (r: Element, i: Int) =>
                model.ModelOrderedLink(
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid,
                  index = i)
              }
            else
              refs.toList.map { case r: Element =>
                model.ModelUnorderedLink(
                  sourceElement = sourceUUID,
                  targetElement = common.EntityUUID(UUIDRegistry.getUUID(r)),
                  metaAssociation = assoc.uuid)
              }
          case _ =>
            Iterable.empty[model.ModelLink]
        }

        links
      }

    (compositeLinks ++ referenceLinks).toVector
  }

}
