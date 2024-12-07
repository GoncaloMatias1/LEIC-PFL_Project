# Replica Board Game Implementation
## FEUP - Functional and Logic Programming (PFL) 2024/2025

### Group Information
| Name             | Student Number | Contribution % | Tasks Performed |
|------------------|---------------|----------------|-----------------|
| Gonçalo Guedes da Conceição | [up202206456]      | [%]            | [Description]   |
| Gonçalo de Abreu Matias | [up202108703]      | [%]            | [Description]   |

### Installation and Execution
1. Ensure SICStus Prolog 4.9 is installed on your system
2. [Additional installation steps to be added]
3. [Execution instructions to be added]

### Game Description
Replica is a strategic two-player board game played on a standard 8x8 chessboard. The game combines elements of checkers with unique movement and transformation mechanics.

#### Components
- 8x8 chessboard
- 12 white flippable checkers
- 12 black flippable checkers

#### Initial Setup
- Players place their pieces in opposite corners
- Each player starts with a 2x2 square in the corner, flanked by a 2x2 square on each side
- The pieces in the very corner start flipped over (indicating kings)

#### Game Rules

##### Movement Types
1. **Step**
   - Move one square forward
   - Can capture enemy pieces by replacement
   - Must move in one of the three directions towards the opponent's corner

2. **Jump**
   - Move in a straight line over friendly pieces
   - Stops at the first empty square or enemy piece
   - Can capture enemy pieces by replacement
   - Must move in one of the three directions towards the opponent's corner

3. **Transform**
   - Convert a regular piece to a king
   - Target piece must be in line-of-sight of a friendly king
   - Enemy pieces block line of sight
   - Does not involve movement

##### Victory Conditions
The game ends when either:
1. A player moves any friendly king to the opposite corner
2. A player captures any enemy king

##### Turn Structure
1. White moves first
2. Players alternate turns
3. One move type per turn (step, jump, or transform)

#### Optional House Rules
Kings can be given special powers such as:
- Moving all kings in one turn
- Double moves for kings
- Backward capture ability

### Source
[Replica on Board Game Geek](https://boardgamegeek.com/boardgame/427267/replica)

[Additional sections for game implementation details, considerations, and conclusions will be added later]