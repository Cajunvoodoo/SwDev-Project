# Purpose
This directory contains the code for the Players and their state.

# Files
- Player.hs
    - Contains representations for player data and connection like `RemotePlayer` and `PlayerData`
- Reachability.hs
    - Contains representation for communication between players and the referee enabling pure and impure interpretations
- Strategy.hs
    - Contains the general representation of a strategy in the game
- KMStrategy.hs
    - Contains the representation of a KarpMiller based strategy
- CardMaximizer.hs
    - Contains the module for the strategy to maximize card draws
- PointMaximizer.hs
    - Contains the module for the strategy to maximize points
- Mechanism.hs
    - Contains functionality for player interactions

# Testing
Please see /README.md.
