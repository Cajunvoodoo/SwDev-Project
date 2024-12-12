# Purpose

Contains the files related to us of vector addition systems to solve coverability.

# Files

- KarpMiller.hs
    - Definitions related to Karp-Miller coverability checking for Equations.
- VASS.hs
    - Defintions related to Vector Addition Systems (with States), an
    isomorphic representation for PebbleSets.

# Testing

To run the test suite, run `/xtest` from the project directory.

Ideally, instead of using `/xtest` directly, one uses `Nix` to create a
devshell, from which one can run `cabal test all` to run the test suite. `nix
build` will also run tests as a part of the build process.

# File Relationship
The KarpMiller uses the VASS to solve coverability equations

![Module Dependency Graph](../Other/modules.png)

Arrows represent 'imports'/'uses'.