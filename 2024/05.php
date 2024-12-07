#!/usr/bin/env nix-shell
<?php
#!nix-shell -i php -p php
$i = stream_get_contents(STDIN);
[$rules, $prints] = explode("\n\n", trim($i));

$dependencies = [];
$dependents = [];
$parsedPrints = [];

foreach (explode("\n", $rules) as $rule) {
    [$dependency, $dependent] = explode("|", $rule);
    $dependencies[$dependent][] = $dependency;
    $dependents[$dependency][] = $dependent;
}

foreach (explode("\n", $prints) as $print) {
    $print = explode(',', $print);

    foreach ($print as $item) {
        if (!isset($dependencies[$item])) $dependencies[$item] = [];
    }
    
    $parsedPrints[] = $print;
}

$part1 = 0;
$incorrectlySorted = [];

foreach ($parsedPrints as $printItems) {
    for ($i = 0; $i < count($printItems); $i++) {
        $ourDependencies = $dependencies[$printItems[$i]] ?? [];
        $matchedDependencies = array_intersect($ourDependencies, $printItems);

        $beforeMe = array_slice($printItems, 0, $i);
        $dependenciesBeforeMe = array_intersect($matchedDependencies, $beforeMe);

        if ($matchedDependencies != $dependenciesBeforeMe) {
            $incorrectlySorted[] = $printItems;
            continue 2;
        }
    }

    $part1 += $printItems[(count($printItems) - 1) / 2];
}

var_dump($part1);

$part2 = 0;

foreach ($incorrectlySorted as $printItems) {
    $remainingDependencies = array_map(fn ($v) => count(array_intersect($v, $printItems)), $dependencies);
    $queue = array_keys(array_filter($remainingDependencies, fn ($c) => $c == 0));
    $sortedPages = [];

    while (!empty($queue)) {
        $current = array_shift($queue);
        $sortedPages[] = $current;

        foreach ($dependents[$current] ?? [] as $dependent) {
            $remainingDependencies[$dependent]--;
            if ($remainingDependencies[$dependent] == 0) {
                $queue[] = $dependent;
            }
        }
    }

    $sortedPrintItems = array_values(array_filter($sortedPages, fn($item) => in_array($item, $printItems)));
    $part2 += $sortedPrintItems[(count($sortedPrintItems) - 1) / 2];
}

var_dump($part2);

