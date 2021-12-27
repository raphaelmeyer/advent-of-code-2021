# Advent of Code 2021

[Advent of Code 2021](https://adventofcode.com/2021)

## Getting Started

### Haskell

Run all tests

    haskell$ stack test

Run all solutions

    haskell$ stack run aoc

Run solution for a specific day

    haskell$ stack run aoc -- --day DAY

Live feedback with _ghcid_

    haskell$ stack exec ghcid

If required install ghcid with `stack install ghcid`.

### C++

Configure build directory

    cpp$ cmake -S . -B _build

Build and run tests

    cpp$ cmake --build _build
    cpp$ cmake --build _build --target test

Run an executable

    cpp$ _build/src/day-01/day-01-exe

## VS Code Setup

Open VS Code workspace `workspace.code-workspace` and install the recommended plugins.

### C++

Use the settings from cmake presets file.

### Haskell

Select _Terminal > Run Tasks..._ for building and running tests.
