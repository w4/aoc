local input = importstr './input';

local parsedInput = [[[std.parseInt(z) for z in std.split(y, '-')] for y in std.split(x, ',')] for x in std.split(input, '\n')];

local part1 = std.sum([
  if ((x[0][0] >= x[1][0] && x[0][1] <= x[1][1]) || (x[1][0] >= x[0][0] && x[1][1] <= x[0][1])) then 1 else 0
  for x in parsedInput
]);

local part2 = std.sum([
  if (x[0][0] <= x[1][1] && x[0][1] >= x[1][0]) then 1 else 0
  for x in parsedInput
]);

{
  part1: part1,
  part2: part2,
}
