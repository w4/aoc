#! /usr/bin/env nix-shell
#! nix-shell -i perl -p perl

$\ = "\n";
$active = 1;
$part1Acc = 0;
$part2Acc = 0;

while ($input = <>) {
  while ($input =~ /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/g) {
    if ($1 eq 'do()') {
      $active = 1;
    } elsif ($1 eq 'don\'t()') {
      $active = 0;
    } elsif ($active) {
      $part2Acc += $2 * $3;
    }

    $part1Acc += $2 * $3;
  }
}

print $part1Acc;
print $part2Acc;
