#!/bin/bash

OLD=$1
NEW=$2

set -e
errcho(){ >&2 echo "$@"; }

mkdir merge-tmp || errcho "rm merge-tmp directory" # will throw error if existant, just remove it
grep -vF -- '--*' "$OLD" | sed -nEe 's/^ *lin +([^ =]+) *=(.*)$/\1	\2/p' | LC_ALL=C sort -k1 > merge-tmp/old-checked-and-formatted.tsv
sed -nEe 's/^ *lin +([^ =]+) *=(.*)$/\1	\2/p' "$NEW" | LC_ALL=C sort -k1 > merge-tmp/new-formatted.tsv
LC_ALL=C join -t '	' -v 2 -j 1 merge-tmp/old-checked-and-formatted.tsv merge-tmp/new-formatted.tsv > merge-tmp/unchecked.tsv
sort -k1 merge-tmp/unchecked.tsv merge-tmp/old-checked-and-formatted.tsv | awk -F '\t' '{res="lin "$1" ="$2; print res}'
rm -r merge-tmp
