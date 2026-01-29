# Functional and Logic Programming Projects
**FEUP - PFL 2024/2025**

This repository contains two projects developed for the Functional and Logic Programming course, showcasing both functional programming in Haskell and logic programming in Prolog.

## 📋 Table of Contents
- [Team Members](#team-members)
- [Project 1: Road Network Analysis (Haskell)](#project-1-road-network-analysis-haskell)
- [Project 2: Replica Board Game (Prolog)](#project-2-replica-board-game-prolog)

---

## 👥 Team Members

| Name | Student Number | Contribution |
|------|----------------|--------------|
| Gonçalo de Abreu Matias | up202108703 | 50% |
| Gonçalo Guedes da Conceição | up202206456 | 50% |

---

## Project 1: Road Network Analysis (Haskell)

A functional programming solution for analyzing road networks, calculating distances, and solving graph-related problems including the Traveling Salesman Problem (TSP).

### 🎯 Features

#### Core Functions
- **`cities`** - Extract all unique cities from a road map
- **`areAdjacent`** - Check if two cities have a direct road connection
- **`distance`** - Calculate distance between two cities
- **`adjacent`** - Find all cities directly connected to a given city
- **`pathDistance`** - Calculate total distance of a path
- **`rome`** - Identify cities with the most connections (hub cities)
- **`isStronglyConnected`** - Verify if all cities are mutually reachable

#### Advanced Algorithms

**Shortest Path (Dijkstra)**
- Implements Dijkstra's algorithm for finding shortest paths
- Uses array-based storage for O(1) access time
- Returns both distance and complete path reconstruction
- Handles disconnected graphs gracefully

**Traveling Salesman Problem**
- Dynamic programming approach with memoization
- Uses adjacency matrix for efficient distance lookups
- Bitmask technique to track visited cities
- Validates graph completeness before solving

### 💡 Implementation Highlights

**Shortest Path Strategy**
```
1. Initialize array with infinite distances (except source = 0)
2. Process cities in order of minimum distance
3. Update distances to adjacent cities
4. Reconstruct path using predecessor tracking
```

**TSP Strategy**
```
1. Build adjacency matrix from road map
2. Use bitmask to represent visited cities
3. Recursively explore all possible paths
4. Return to starting city for valid solution
```

### 🔧 Task Distribution

**Gonçalo de Abreu Matias**
- `cities`, `areAdjacent`, `adjacent`
- `isStronglyConnected`, `travelSales`

**Gonçalo Guedes da Conceição**
- `distance`, `pathDistance`, `rome`
- `shortestPath`, auxiliary functions

---

## Project 2: Replica Board Game (Prolog)

A strategic two-player board game implementation combining elements of checkers with unique transformation mechanics. Players compete on an 8×8 board with the goal of either capturing the opponent's king or reaching the opponent's corner with their own king.

### 🎮 Game Rules

#### Setup
- 8×8 chessboard
- 12 pieces per player (11 regular + 1 king)
- White starts in bottom-right, Black in top-left

#### Movement Types
1. **Step** - Move one square forward
2. **Jump** - Leap over friendly pieces (multiple squares)
3. **Transform** - King converts regular piece to king

#### Victory Conditions
- Move your king to opponent's corner square, **OR**
- Capture opponent's king

### 🎯 Features

#### Game Modes
- **Human vs Human** - Local two-player mode
- **Human vs Computer** - Play against AI
- **Computer vs Human** - AI plays first
- **Computer vs Computer** - Watch AI battle

#### AI Implementation
- **Level 1** - Random valid moves
- **Level 2** - Greedy strategy prioritizing:
  - King captures
  - Regular piece captures
  - Offensive positioning
  - Defensive moves

### 🖥️ Technical Details

#### Game State Representation
```prolog
state(Board, Player, GameConfig)
```
- **Board**: 8×8 matrix
- **Player**: `white` or `black`
- **GameConfig**: Mode and AI settings

#### Piece Notation
- `empty` - Empty square
- `w` / `b` - White/Black regular piece
- `wk` / `bk` - White/Black king

#### Move Format
```prolog
(Row, Column, Direction)
```
- Coordinates: 1-8
- Directions: `H` (Horizontal), `V` (Vertical), `D` (Diagonal), `T` (Transform)

### 🚀 Installation & Execution

#### Requirements
- SICStus Prolog 4.9

#### Steps
1. Install SICStus Prolog 4.9
2. Navigate to project directory
3. Launch SICStus Prolog
4. Load game: `consult('game.pl').`
5. Start playing: `play.`

### 🔧 Task Distribution

**Gonçalo de Abreu Matias**
- Menu system and board display
- Move validation (Black player)
- King capture mechanics
- Piece transformation logic
- Game state examples and documentation

**Gonçalo Guedes da Conceição**
- Move piece logic and selection
- Move validation (White player)
- Game over detection
- AI implementation (Levels 1 & 2)
- Board evaluation function

### 🔮 Future Improvements

#### Replica Game
- Advanced AI with strategic depth
- Corner-focused king positioning
- Capture avoidance heuristics
- Graphical user interface
- Game state save/load
- Undo/redo functionality

#### Road Network
- Variable board sizes
- Parallel algorithm optimization
- Visualization of paths and networks

---

## 📚 References

### Project 1 (Haskell)
- Haskell Documentation
- Algorithm Design Manual - Skiena
- Dynamic Programming Techniques

### Project 2 (Prolog)
- [SICStus Prolog Documentation](https://sicstus.sics.se/documentation.html)
- [Replica on Board Game Geek](https://boardgamegeek.com/boardgame/427267/replica)

---

## 📄 License

Academic projects developed for educational purposes at FEUP.

**Course**: Functional and Logic Programming (PFL)  
**Academic Year**: 2024/2025  
**Institution**: Faculty of Engineering, University of Porto
