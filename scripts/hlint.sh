#!/usr/bin/env bash

set -euo pipefail

# This script is used to lint the source code of the project using hlint.
# It is meant to be run from the root of the project.

hlint tasty-json-reporter tasty-json-reporter-test
