# Purpose
This directory contains the planning and source code for the Bazaar game
project. Please see the README for the individual directories for more detailed
information.

# Files
- Common/
    - Contains the source files for the common game elements featured in Bazaar.
- Common.hs
    - Re-exports of most of Common/.
- Player/
    - Contains the definitions of types relevant to players, such as the
    PlayerState datatype. Also contains the code for strategies.
- State/
    - Contains game-state definitions, such as GameState and TurnState.
- Planning/
    - Contains the planning documents for the Bazaar Game.
- Referee/
    - Contains referee funtionality
- Other/
    - Contains miscellaneous files, such as static images.

# Testing
N/A, See ../README.md for information on testing its files.

# File Relations
The planning documents in Planning/ cover the development over the project as
reflected in the source code in Common/. Files in Other/ are used as assets for
the image-related code and the memos in Planning/.
