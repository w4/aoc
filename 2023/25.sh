#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash -p graphviz -p imgcat -p gnused -p gawk -p coreutils -p bc

set -euo pipefail

function log() {
  echo -e "\033[34m$1\033[0m" >&2
}

function logc() {
  echo -e "+ \033[35m$*\033[0m" >&2
  "$@"
}

log "Generating graph visualisation using graphviz"
# shellcheck disable=SC2016
GRAPH=$(logc awk -F': ' '{split($2, nodes, " "); for (i in nodes) print $1" -- "nodes[i]}' input)
echo "graph a { $GRAPH }" | logc neato -Tpng | logc imgcat

while true; do
  read -r -p "Which edge would you like to cut? (eg. AA -- BB) " ans

  if [ "$ans" = "" ]; then
    break
  elif echo "$GRAPH" | grep -q "$ans"; then
    GRAPH="${GRAPH//$ans/}"
    echo "graph a { $GRAPH }" | logc neato -Tpng | logc imgcat
  else
    echo "No match for $ans"
  fi
done

log "Calculating graph structure"
SPLIT=$(echo "graph a { $GRAPH }" | ccomps -x || true)

if [ "$(echo "$SPLIT" | grep -o "graph" | wc -l)" -ne 2 ]; then
  echo "There's not two distinctive subgraphs, try again"
  exit 1
fi

# shellcheck disable=SC2016
PART1=$(echo "$SPLIT" \
  | logc awk '/^graph/ {subgraph=$2} /^[[:space:]]+/ {gsub(/;/, "", $1); gsub(/;/, "", $3); print subgraph, $1; print subgraph, $3}' \
  | logc sort \
  | logc uniq \
  | logc cut -d ' ' -f 1 \
  | logc uniq -c \
  | tr '\n' ' ' \
  | logc sed -E 's/^[[:space:]]+([0-9]+)[[:space:]]+[^ ]+[[:space:]]+([0-9]+).*/\1*\2\n/g' \
  | bc)

echo "$PART1"
