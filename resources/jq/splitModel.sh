#!/bin/bash

mkdir tables

cat model.json | jq -c '.elements[]' > tables/ModelElements.json
cat model.json | jq -c '.links[] | select(.type == "ModelOrderedLink") | {sourceElement,targetElement,metaAssociation}' > tables/ModelOrderedLinks.json
cat model.json | jq -c '.links[] | select(.type == "ModelUnorderedLink") | {sourceElement,targetElement,metaAssociation}' > tables/ModelUnorderedLinks.json
cat model.json | jq -c '.appliedStereotype[]' > tables/AppliedStereotypes.json
cat model.json | jq -c '.appliedStereotypePropertyReferences[] | select(.type == "AppliedStereotypePropertyUnorderedReference") | {modelElement,associationTargetEnd,referencedElement}' > tables/AppliedStereotypePropertyUnorderedReferences.json
cat model.json | jq -c '.appliedStereotypePropertyReferences[] | select(.type == "AppliedStereotypePropertyOrderedReference") | {modelElement,associationTargetEnd,referencedElement,index}' > tables/AppliedStereotypePropertyOrderedReferences.json
cat model.json | jq -c '.elementAttributeValues[] | select(.type == "ModelElementOrderedAttributeValue") | {modelElement,attributeValue,index}' > tables/ModelElementOrderedAttributeValues.json
cat model.json | jq -c '.elementAttributeValues[] | select(.type == "ModelElementUnorderedAttributeValue") | {modelElement,attributeValue}' > tables/ModelElementUnorderedAttributeValues.json
cat model.json | jq -c '.instantiatedMetamodels[]' > tables/InstantiatedMetamodels.json
cat model.json | jq -c '.appliedProfiles[]' > tables/AppliedProfiles.json
cat model.json | jq -c '.resource[]' > tables/Resource.json
