# Purpose
This directory contains the documents for the server side code for running the referee game

# Files
- Server.hs
    - Contains the main server logic for receiving player connections and starting games.
- Player.hs
    - Contains the referee view of the client

# Testing
To run the test suite, run `/xtest` from the project directory.

Ideally, instead of using `/xtest` directly, one uses `Nix` to create a
devshell, from which one can run `cabal test all` to run the test suite. `nix
build` will also run tests as a part of the build process.

# File Relationship

Someone will run `Server.hs`. `Server.hs` will ask `Player.hs` when it needs to send/receive any information from the server (though mostly handled directly by `Mechanism.hs` via the `Reachability` type).