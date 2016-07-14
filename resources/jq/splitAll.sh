#!/bin/bash

case "$OS" in
  Darwin)
    READLINK=greadlink
    ;;
  *)
    READLINK=readlink
    ;;
esac

TOOLS=$(${READLINK} -f $(dirname $0))

echo "# TOOLS=$TOOLS"

files=`find . -name '*.json'`

for f in $files; do
  pushd $(dirname $f) 2>&1 > /dev/null
  echo "# `pwd`"
  case "$(basename $f)" in
    model.json)
      ${TOOLS}/splitModel.sh
      ;;
    profile.json)
      ${TOOLS}/splitProfile.sh
      ;;
  esac
  popd 2>&1 > /dev/null
done

