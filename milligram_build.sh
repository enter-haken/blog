#!/bin/sh

NODE_PACKAGEMANAGER="yarn";

if [ ! -f milligram/dist/milligram.min.css ]; then
  cd milligram;
  if [ ! -d milligram/node_modules ]; then
    $NODE_PACKAGEMANAGER install
  fi
  $NODE_PACKAGEMANAGER build
  cd ..								
fi


