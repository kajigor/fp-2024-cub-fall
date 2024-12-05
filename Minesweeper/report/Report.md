# Minesweeper Game Project Report

## Task Description

The goal of this project was to implement a fully functional Minesweeper game using Haskell, adhering to the principles of functional programming. The project requirements were:

- **Console-based User Interface:** Allow players to interact with the game through text commands.
- **Grid Representation and Initialization:** Create a grid with randomized mine placement and accurate adjacency indicators.
- **Game Logic:** Implement the core mechanics, including revealing cells, recursive exploration, win/loss conditions, and flagging.
- **Error Reporting:** Handle invalid user inputs gracefully, ensuring smooth gameplay.
- **Testing:** Develop unit and property-based tests to validate the game’s functionality and reliability.

This project was a test of applying theoretical concepts from the course to a practical problem, emphasizing modular design, recursion, and immutability.

---

## Architecture of the Solution

### Modular Breakdown

The solution was designed with clear separation of concerns across multiple modules:

- **Grid Module:** Responsible for grid creation, random mine placement, and maintaining cell states.
- **Game Logic Module:** Handled gameplay mechanics, including recursive exploration of adjacent cells and win/loss conditions.
- **Console UI Module:** Rendered the game grid in a text-based interface and processed player commands.
- **Error Handling Module:** Validated user inputs and ensured clear reporting of errors without interrupting the gameplay loop.
- **Testing Modules:** Contained unit and property-based tests to verify individual functions and game behavior.

### Execution Flow

1. **Grid Initialization:** The game grid is initialized with random mine placement. Pure functions calculate the number of adjacent mines for each cell.
2. **Gameplay Loop:** Players issue commands (`reveal`, `flag`, `unflag`), which are validated and applied to the game state. After each move, the updated grid is displayed.
3. **End Conditions:** The game ends with a win if all non-mine cells are revealed, or a loss if a mine is uncovered.
4. **Error Handling:** Invalid commands are intercepted and explained, ensuring continuity in gameplay.

---

## Architectural Decisions

### Why This Architecture?

- **Modular Design:** Dividing the game into modules ensured clarity, ease of debugging, and extensibility. Each module was designed to focus on a specific responsibility, minimizing interdependencies.
- **Immutability:** Functional programming principles inspired a design where the game state is never modified but instead recreated with each user action. This approach simplified reasoning about state changes and facilitated testing.
- **Recursive Functions:** Recursive patterns were used to handle tasks like revealing adjacent cells, aligning with the functional programming paradigm.
- **Error Handling:** Errors were managed using types like `Maybe` and `Either`, ensuring robust handling of invalid inputs.

---

## Library Choices

### Why These Libraries?

- **`System.Random`:** Used for generating random mine positions, encapsulating the randomness in a controlled monadic context. This allowed pure functions to remain separate from side effects.
- **`Text.IO`:** Provided straightforward input and output capabilities for the console-based user interface.
- **`Hedgehog`:** Selected for property-based testing, as it allowed for the generation of random scenarios to validate the game’s behavior and ensure robustness under edge cases.
- **`QuickCheck`:** Supported unit testing, enabling comprehensive verification of individual functions.

---

## Investigation of Performance

### Strengths

- **Efficiency in Grid Operations:** Recursive exploration of empty cells was optimized to track visited cells, preventing redundant checks and ensuring linear performance relative to the grid size.
- **Scalability:** The implementation scaled well for grids up to 20x20. Response times remained consistent even with complex grids and higher mine densities.
- **Testing Coverage:** Comprehensive property-based testing provided confidence in the implementation, uncovering edge cases not easily detected through manual testing.

### Challenges

- **Random Mine Placement:** Generating unique mine positions could become a bottleneck for larger grids with high mine density. Optimizations included using efficient list operations to ensure uniqueness without excessive recomputation.
- **Console Rendering:** Larger grids introduced slight delays in rendering due to terminal constraints. However, this did not significantly impact the user experience.

---

## Conclusion

This Minesweeper game project demonstrates the effective application of Haskell’s functional programming principles to a practical problem. The modular architecture, emphasis on immutability, and recursive solutions aligned with the course’s objectives. Comprehensive testing ensured correctness and reliability, while efficient design choices supported scalability and performance. Overall, this project represents a strong understanding of functional programming and the practical capabilities of Haskell.
