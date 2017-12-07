#!/usr/bin/env bash

# Upload a file or directory both to s3 and to IPFS.

set -e

BUCKET=ipfs-archive-backups

trace () {
  echo "$@"
  "$@"
}

for input in "$@"
do tmp="$(realpath $(mktemp ./.tmp.XXXXXX))"

   tar c "$input" | gzip > "$tmp"

   hash=$((sha256sum | sed 's/ .*//') <"$tmp")

   /usr/bin/aws s3 cp "$tmp" "s3://$BUCKET/blob/$hash"

   ipfs add -r "$input"

   # rm "$tmp"
done
