#!/bin/bash

cat UML.metamodel.json | jq -c '.classifiers[] | select(.type == "MetaClass") | {uuid,name}' > UML/UML.metaclasses.json
