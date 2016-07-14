#!/bin/bash

mkdir tables

cat profile.json | jq -c '.classifiers[]' > tables/stereotypes.json
cat profile.json | jq -c '.associationTargetEnds[] | select(.type == "AssociationTargetReferenceEnd") | {uuid,name}' > tables/associationTargetReferenceEnds.json
cat profile.json | jq -c '.associationTargetEnds[] | select(.type == "AssociationTargetCompositeEnd") | {uuid,name}' > tables/associationTargetCompositeEnds.json
cat profile.json | jq -c '.attributes[]' > tables/Attributes.json
cat profile.json | jq -c '.featureLowerBounds[]' > tables/featureLowerBounds.json
cat profile.json | jq -c '.featureUpperBounds[]' > tables/featureUpperBounds.json
cat profile.json | jq -c '.featureOrdering[]' > tables/featureOrdering.json
cat profile.json | jq -c '.importedLibraries[]' > tables/importedLibraries.json
cat profile.json | jq -c '.extendedMetamodels[]' > tables/extendedMetamodels.json
cat profile.json | jq -c '.importedProfiles[]' > tables/importedProfiles.json
cat profile.json | jq -c '.generalizations[]' > tables/generalizations.json
cat profile.json | jq -c '.extendedMetaclass[]' > tables/extendedMetaclass.json
cat profile.json | jq -c '.stereotype2attribute[]' > tables/stereotype2attribute.json
cat profile.json | jq -c '.attribute2type[]' > tables/attribute2type.json
cat profile.json | jq -c '.stereotype2associationEndMetaClassProperty[]' > tables/stereotype2associationEndMetaClassProperty.json
cat profile.json | jq -c '.stereotype2associationEndStereotypeProperty[]' > tables/stereotype2associationEndStereotypeProperty.json
cat profile.json | jq -c '.resource[]' > tables/resource.json
