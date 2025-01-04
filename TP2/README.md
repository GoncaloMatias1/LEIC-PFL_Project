# Replica Board Game Implementation
## FEUP - Functional and Logic Programming (PFL) 2024/2025

### Group Information
| Name             | Student Number | Contribution % | Tasks Performed |
|------------------|---------------|----------------|-----------------|
| Gonçalo Guedes da Conceição | up202206456 | 50% | Get piece based on move, White Player 1 move validation, PC level 1 and level 2 implementation|
| Gonçalo de Abreu Matias | up202108703 | 50% |  Menu Implementation, Board Implementation, Move validation, Black Player 2 move validation, King pieces capture, Game Over, Transform Friend piece into king |

### Installation and Execution
#### Windows
1. Install SICStus Prolog 4.9 following the official installation guide
2. Navigate to the game directory
3. Launch SICStus Prolog
4. Consult the main game file: `consult('game.pl').`
5. Start the game by typing: `play.`

#### Linux
1. Install SICStus Prolog 4.9 using your distribution's package manager
2. Follow the same steps 2-5 as Windows

### Game Description
Replica is a strategic two-player board game played on an 8x8 chessboard. The game combines elements of checkers with unique movement and transformation mechanics.

#### Basic Rules
- Players start with 12 pieces each (11 regular pieces and 1 king)
- Pieces move in three ways:
  1. Step: One square forward
  2. Jump: Multiple squares over friendly pieces
  3. Transform: Convert regular piece to king

#### Victory Conditions
- Moving a king to the opponent's corner
- Capturing the opponent's king

[vou meter aqui imagens]

#### Information Sources
- [Replica on Board Game Geek](https://boardgamegeek.com/boardgame/427267/replica)
- Official documentation from designer

### Game Extension Considerations
1. **Variable Board Sizes**
   - Current implementation uses 8x8 board
   - Code structured to allow future board size modifications
   - Movement calculations independent of board size

2. **Optional Rules**
   - Framework in place for additional king powers
   - Simplified ruleset for beginners possible
   - Advanced rules for experts can be added

### Game Logic
#### Game Configuration Representation
The game configuration is represented using the following structure:
```prolog
state(Board, Player, GameConfig)
```
- Board: 8x8 matrix representing the game board
- Player: current player (white/black)
- GameConfig: game mode and AI configuration

#### Internal Game State Representation
Pieces are represented as:
- `empty`: Empty square
- `w`: White piece
- `b`: Black piece
- `wk`: White king
- `bk`: Black king

Example states:
```prolog
% Initial state snippet
[empty,empty,empty,empty,empty,empty,empty,empty],
[bk,b,empty,empty,empty,empty,empty,empty],
...

% Intermediate state snippet
[empty,empty,w,empty,empty,empty,empty,empty],
[empty,bk,empty,b,empty,empty,empty,empty],
...

% Final state snippet
[wk,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
...
```

#### Move Representation
Moves are represented as:
```prolog
(PieceRow, PieceColumn, Direction)
```
- PieceRow, PieceColumn: Coordinates (1-8)
- Direction: 'H' (Horizontal), 'V' (Vertical), 'D' (Diagonal), 'T' (Transform)

#### User Interaction
- Menu-driven interface with clear options
- Input validation for:
  - Menu selections (1-4)
  - Move coordinates (1-8)
  - Movement direction
- Error messages for invalid inputs
- Board visualization after each move

### Conclusions
#### Current Limitations
- Basic AI implementation
- Limited undo/redo functionality
- No game state saving

#### Future Improvements
1. Enhanced AI with deeper strategic thinking
2. GUI implementation

### Bibliography
1. [SICStus Prolog Documentation](https://sicstus.sics.se/documentation.html)
2. [Replica Board Game Official Rules](https://boardgamegeek.com/boardgame/427267/replica)