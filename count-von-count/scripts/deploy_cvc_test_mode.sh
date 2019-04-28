#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

stack runghc scripts/teams2019.hs

count-von-count

