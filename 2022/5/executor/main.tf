locals {
  remainingInstructions = length(var.instructions) == 0 ? [] : slice(var.instructions, 1, length(var.instructions))
  currentInstruction = length(var.instructions) == 0 ? null : split(",", var.instructions[0])
  amount = local.currentInstruction == null ? null : tonumber(local.currentInstruction[0])
  fromStack = local.currentInstruction == null ? null : (tonumber(local.currentInstruction[1]) - 1)
  toStack = local.currentInstruction == null ? null : (tonumber(local.currentInstruction[2]) - 1)
  setFromBlocks = local.currentInstruction == null ? null : slice(var.stackedBlocks[local.fromStack], local.amount, length(var.stackedBlocks[local.fromStack]))
  newToBlocks = local.currentInstruction == null ? null : slice(var.stackedBlocks[local.fromStack], 0, local.amount)
  newToBlocksMaybeReversed = var.reverse ? reverse(local.newToBlocks) : local.newToBlocks
  setToBlocks = local.currentInstruction == null ? null : concat(local.newToBlocksMaybeReversed, var.stackedBlocks[local.toStack])
  finalBlocks = local.currentInstruction == null ? var.stackedBlocks : [for i in range(length(var.stackedBlocks)) : (i == local.fromStack ? local.setFromBlocks : (i == local.toStack ? local.setToBlocks : var.stackedBlocks[i]))]
}

variable "stackedBlocks" {}
variable "instructions" {}
variable "reverse" {}

output "finalBlocks" {
  value = local.finalBlocks
}

output "remainingInstructions" {
  value = local.remainingInstructions
}
