#!/bin/sh

NODE_PACKAGEMANAGER="yarn";

set -e;

if [  -z "$(ls -A ./milligram)" ]; then 
  # a hook causes an error, which can be ignored
  git submodule update --init --recursive || true;
fi

# due to the build artefacts are part of the repo 
# this part will be interesting after a first 
# "make deep_clean"
#
# this is a kind of health check of the repo.
# npm / yarn install && npm / yarn build should work properly

if [ ! -f milligram/dist/milligram.min.css ]; then
  cd milligram;
  if [ ! -d milligram/node_modules ]; then
    $NODE_PACKAGEMANAGER install
  fi
  $NODE_PACKAGEMANAGER build
  cd ..								
fi
