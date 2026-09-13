#!/usr/bin/env bash
# Compares the JSON produced by reading fixtures/20201200.onix through the
# generated Go client against the committed snapshot. A difference means the
# generated client's runtime behaviour changed.
#
# To accept a new snapshot:
#   npx bazelisk build //e2e/go:snapshot
#   cp bazel-bin/e2e/go/out.json fixtures/20201200.json
set -o errexit
set -o nounset
set -o pipefail

actual="$1"
expected="$2"

if ! diff -u "$expected" "$actual"; then
  echo "" >&2
  echo "The generated Go client no longer reproduces fixtures/20201200.json." >&2
  echo "If the change is intended, refresh the snapshot as described in" >&2
  echo "e2e/go/snapshot_test.sh." >&2
  exit 1
fi
