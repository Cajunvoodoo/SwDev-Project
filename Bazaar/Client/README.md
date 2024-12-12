# Purpose
This directory contains the documents for the client side code for the Bazaar Game

# Files
- Client.hs
    - Contains the main client logic.
- Referee.hs
    - Contains the client view of the referee

# Testing
To run the test suite, run `/xtest` from the project directory.

Ideally, instead of using `/xtest` directly, one uses `Nix` to create a
devshell, from which one can run `cabal test all` to run the test suite. `nix
build` will also run tests as a part of the build process.

# File Relationship

Someone will run `Client.hs`. `Client.hs` will ask `Referee.hs` when it needs to send/receive any information from the server.