#!/usr/bin/env bash

## convert-xz-to-gzip.bash:
## Create a copy of XZ compressed files with gzip compression instead.
## Run this script from the repository's root directory.

shopt -s nullglob
shopt -s globstar

for xz_path in **/*.xz; do
    echo >&2 $xz_path
    # use --no-name for reproducible output
    xz --decompress --stdout "$xz_path" | gzip --no-name --best > "${xz_path%.xz}.gz"
done
