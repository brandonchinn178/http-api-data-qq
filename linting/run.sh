#!/usr/bin/env bash

set -eo pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}/.."

NAME=$1
shift

BIN_DIR="$("${HERE}/stack.sh" path --local-bin)"
exec "${BIN_DIR}/${NAME}" "$@"
