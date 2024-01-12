# The Rootbotics Assistant - Take the thinking out of managing automated factions!

## About
The Rootbotics Assistant is based on the following expansions for the board game
[Root](https://ledergames.com/products/root-a-game-of-woodland-might-and-right):
- [The Clockwork Expansion](https://ledergames.com/products/root-the-clockwork-expansion)
- [The Clockwork Expansion 2](https://ledergames.com/products/root-the-clockwork-expansion-2?variant=42703095038173)

The Rootbotics Assistant exists to alleviate the stress and complexity that might be
had while managing the automated factions by handling the turn progression and
some of the decision analysis for the user. **Note:** this tool is used entirely in
the terminal and lacks a GUI implementation!

The Rootbotics Assistant is currently in an unfinished state. 
The following features are finished/in testing:
- Logical Lizards (no traits or difficulty yet)
- Automated Alliance (no traits or difficulty yet)
At this time, the following features are remaining for implementation:
- Mechnical Marquise 2.0
- Electric Eyrie
- Vagabot
- Logical Lizards
- Riverfolk Robots
- Drillbit Duchy
- Difficulties and Traits
- Lake Map Raft Logic

## Installation
### Requirements
- Alire ([Ada LIbrary REpository](https://alire.ada.dev/))

In the project root directory:
```
$ alr build
```
## Understanding the Interface
Because the interface is restricted to the terminal, some elements
are non-trivial to understand. The following describes what certain
design constructs represent.

### Map - Clearings
```
                Rule? --       -- Token? (Yes = 'T', No = '@')
                         \   /
Number of Buildings --- 2-^-@    - Number of Warriors
                        | 5 | -/
               Suit --- F---1 --- Priority
```

## Credits
- Cole Wehrle - The creation of Root
- Kyle Ferrin - The Root artwork
- Benjamin Schmauss - The creator of Root: The Clockwork Expansion 1 & 2
- Leder Games - The distributor and producer of Root
