#!/usr/bin/env bash

set -eo pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec stack --stack-yaml "${HERE}/stack-linters.yaml" "$@"
