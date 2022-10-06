#!/usr/bin/env bash

set -euo pipefail

# This script is used to format the source code of the project using ormolu.
# It is meant to be run from the root of the project.

find tasty-json-reporter tasty-json-reporter-test -name \*.hs -exec ormolu --mode inplace {} \;
