#!/usr/bin/env bash

STDIN=$(cat -)

PART1=$(curl --max-redirs 100000 -Ls "http://127.0.0.1:8888/part1/$STDIN" -o /dev/null -w %{url_effective})
echo $(echo $PART1 | gsed -E 's/.*\/([1]+)/\1/' | tr -d '\n' | wc -c)

PART2=$(curl --max-redirs 100000 -Ls "http://127.0.0.1:8888/part2/$STDIN" -o /dev/null -w %{url_effective})
echo $(echo $PART2 | gsed -E 's/.*\/([1]+)/\1/' | tr -d '\n' | wc -c)
