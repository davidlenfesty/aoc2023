# Advent of Code 2023

I'm doing it in scheme this year

I'm using the chez scheme compiler.

## Running/random notes

(For now) I'm not tracking the inputs, so you'll have to make an `inputs` directory with the contents being a set of files named `dayX`.

My rework for day 1 part 2 didn't allow for running both parts, and I didn't bother to do any re-work. Checkout the tag `day1/part1` to see that solution.

Day 1 also uses stdin, so to run it: `cat inputs/day1 | chez --script day1.scm`.