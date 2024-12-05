# Project Report

[Ana-Maria Dobrescu]
[5.12.2024]

---

## 1. Task Description

My task for the final project was to create a Minesweeper game in Haskell, and I chose to implement it as a console-based game for users to play. The project needed to satify the following requirements:

- represent and initilize the game grid with mines
- include basic gameplay features:
  - revealing cells
  - flagging and unflagging cells
  - handling win and loss conditions
  - quiting the game
- display the board after each action
- provide a functional user interface
- provide unit and property-based tests

The project had to be implemented individually, meaning I wasn't allowed to collaborate with my colleagues.

---

## 2. Architecture Of My Solution

My project follows a modular design, separating the core logic, game grid operations, and user interaction into distinct components. This ensures clarity and maintainability. This separation also allows easier debugging and testing as the modules can be independently modified.

### 1. `Minesweeper.hs`

This module manages the core functionality of the game, aencapsulating all the logic and state management. Key features include:

- **Game State Representation**: it defines the `Minesweeper` data type, which tracks the board's dimensions, mines' location, and the state of each cell (revealed, hidden, flagged)
- **Game Initialization**: `initGame` generates a grid of the specified size with mines placed randomly
- **Game Logic**: `makeMove`, `isMine`, and `nearbyMines` handle game actions and provide the logic for the gameplay
- **Rendering**: `printBoard` and `printFinalBoard` dynamically display the board during and after the game

### 2. `Main.hs`

This module manages user interaction, serving as the entry point for the application and it handles the following:

- **Game Flow Management**: controls the sequence of the game, including initialization, user input processing, and the game loop execution
- **Input Parsing**: parses user commands and translates them into actions such as revealing or flagging a cell.
- **Error Handling**: ensures the validity of the input

---

## 3. Architecture Decisions

### Modular Design

The game was divided into distinct modules to promote clarity, maintainability, and scalability. By separating core logic (`Minesweeper.hs`) from user interaction (`Main.hs`), each module focuses on a single responsibility:

- core logic is independent of the user interface, making it easier to test and reuse
- the user interface is lightweight, with minimal logic

### Data Structures

- **Sets**: I used `Data.Set` to track mines, revealed cells, and flagged cells, as sets are effecient for these kind of operations
- **Tuples**: `(Int, Int)` tuples were used to represent cell positions due to their simplicity

### Console Output

I chose a control-based interface for simplicity and ease of implementation for both the developer and the user.

### Error Handling in the Game Loop

The game loop combines `Maybe`and pattern matching and handle user input and game state transitions. The `parseInput` function returns a `Maybe` value to implement either valid commands or invalid input. The combination ensures smooth gameplay for the user, as it validates the inputs and guides the players further, while avoiding errors and crashes.

---

## 4. Library Choices

- **`Data.Set`**: chosen due to the efficiency of implementing of set operations as insertion, deletion, and membership checks, which helps in tracking game states
- **`System.Random`**: used for random mine placement during the game initialization so that each game has a different setup
- **`Test.HUnit` and `Test.QuixkCheckup`**: used for testing
  - `HUnit` - used for unit tests, ensuring individual functions behave as they should
  - `QuickCheck` - used for property-based testing

---

## 5. Investigation of Performance

### Observations

- The project scales well and is efficient with typical grid sizes. However, the performance might degrade for large grids (e.g. 100x100), or high density of mines.
- The random placement of mines is efficient, even for larger boards. I tried ensuring a gameflow by making sure the first move doesn't result in a loss (avoid placing mines in the starting cell (0,0)).
- `printBoard` operates linearly with respect to the number of cells.

---

## Conclusion

The Minesweeper project allowed me to apply functional programming principles to create a robust and modular game. By using efficient data structures and libraries, I ensured that the game meets the requirements for both functionality and performance. Overall, this project was a great opportunity to combine problem-solving, programming, and testing skills.
