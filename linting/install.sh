#!/usr/bin/env bash

set -eo pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec "${HERE}/stack.sh" install "$@" fourmolu hlint
