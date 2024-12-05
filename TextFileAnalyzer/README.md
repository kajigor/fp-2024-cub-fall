
**What the task was.**  
**Text File Analyzer**

-  Console (or other user) interface. ✅
-  Read text files and compute statistics (word count, line count, character count). ✅
-  Identify and display the most frequent words. ✅
-  Implement n-gram analysis for sequences of words. ✅
-   Generate a textual word cloud representation. ✅
-   Error reporting for file access issues or invalid data or commands. ✅
-   Unit and property-based tests. ✅

**Architecture of the Solution**  

1.  **Library Modules (src/):**
    
    -   **ErrorHandling:** Manages error types and user-friendly reporting.
    -   **FileReader:** Handles reading, validating, and checking text files.
    -   **Statistics:** Calculates basic statistics
    -   **FrequentWords:** Identifies and ranks frequently words.
    -   **NGramAnalysis:** Generates and analyzes n-grams.
    -   **WordCloud:** Prepares data for creating word clouds.
    -   **TextUtils:**  utility functions for tokenization, normalization, and cleaning.
    -   **Interface:** CLI interactions.
2.  **Executable Module (app/Main.hs):**
    
    -   **Main:** Entry point for the CLI, coordinating user input, library calls, and results display.
3.  **Test Suite (test/):**
    
    -   **Spec.hs:** Aggregates all test cases.
    -   **Individual Spec Modules:** 

**Architectural Decisions**  
1.  **Modular Design:** Simplifies maintenance and testing by assigning specific tasks to distinct modules.
2.  **CLI over GUI:** Shifted focus from GUI to CLI due to complexity and time constraints with gi-gtk bindings.

**Library Choices**  
Libraries were selected for efficiency and suitability:

7.  **base:** Core library essential for Haskell projects.
8.  **text:** Optimized for Unicode text processing, outperforming standard String types.
9.  **containers:** Provides efficient data structures like Map and Set for word frequency and n-gram analysis.
10.  **directory:** For file validation and access.
11.  **gi-gtk (Initially):** GUI bindings, later abandoned for CLI simplicity.
12.  **Testing Libraries (HUnit, QuickCheck, silently):** Ensures reliability through unit and property-based tests.

**Performance Investigation**  

 Benchmark: `text-file-analyzer-bench`

 FileReader: `readTextFile`
- **Time**: 9.296 μs (9.184 μs .. 9.397 μs)
- **R²**: 0.999 (0.998 R² .. 0.999 R²)
- **Mean**: 9.247 μs (9.140 μs .. 9.352 μs)
- **Standard Deviation**: 349.0 ns (294.0 ns .. 430.0 ns)
- **Variance Introduced by Outliers**: 46% (moderately inflated)

---

Statistics: `computeStatistics`
- **Time**: 4.613 ns (4.508 ns .. 4.806 ns)
- **R²**: 0.977 (0.931 R² .. 1.000 R²)
- **Mean**: 4.724 ns (4.566 ns .. 5.256 ns)
- **Standard Deviation**: 888.7 ps (104.3 ps .. 1.809 ns)
- **Variance Introduced by Outliers**: 98% (severely inflated)

---

NGramAnalysis: `generateNGrams (n=3)`
- **Time**: 6.991 s (6.423 s .. NaN s)
- **R²**: 0.999 (0.999 R² .. 1.000 R²)
- **Mean**: 6.864 s (6.725 s .. 6.966 s)
- **Standard Deviation**: 134.4 ms (7.298 ms .. 163.9 ms)
- **Variance Introduced by Outliers**: 19% (moderately inflated)

**Usage**
1. `stack exec text-file-analyzer <file-path\>` for work with your file
2. `stack bench` for benchmarking
3. `stack test` for tests

