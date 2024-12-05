
# Scientific Calculator

This project implements a **scientific calculator** with support for a wide range of mathematical operations, constants, error handling, and functionalities. Below is an overview of its capabilities, how to run it, and the testing suite used to ensure its robustness.

---

## How to Run the Calculator

### Running the Calculator
To run the scientific calculator, use:

```bash
stack run
```

### Running Tests
To execute the test suite:

```bash
stack test
```

### Generating an Executable
To build an executable file:

```bash
stack build
stack exec ScientificCalculator-exe
```

---

## Supported Functionalities

### 1. Expression Evaluation
The `=` symbol evaluates a given mathematical expression and updates the last result **only if the evaluation is successful**.

Example:
```text
> =
Enter an expression to evaluate:
sin (π/2)
Result: 1.0
```

### 2. Memory Operations
- **Default Values:**
  - Memory starts at `0.0`.
  - Last result starts at `0.0`.

- **Operations:**
  - `ac` clears the last result (sets it to `0.0`).
  - `mc` clears the memory (sets it to `0.0`).
  - `m+` adds the last result to memory.
  - `m-` subtracts the last result from memory.
  - `mr` retrieves the value in memory.

### 3. Chained Operations with Last Result
The calculator allows the application of an expression to the last result in three distinct cases:

#### Case 1: Starting with a Unary Function
Applies the unary function to the last result and evaluates the remaining expression.

Example:
```text
> =
Enter an expression to evaluate:
π/2
Result: 1.5707963267948966
> sin + 6
Result: 7.0
```
In this example we first apply the `sin` to `π/2` then we sum the result with 6

#### Case 2: Starting with a Binary Operator
Uses the last result as the left operand and evaluates the rest of the expression.

Example:
```text
> =
Enter an expression to evaluate:
π/2
Result: 1.5707963267948966
> sin + 6
Result: 7.0
> + 9 - 5
Result: 11.0
```
In this example we take the resulting `7` add `9` to it and then subtract `5` from the result.

#### Case 3: Starting with a Number or Constant
Evaluates the entire expression and updates the last result.

Example:
```text
> =
Enter an expression to evaluate:
π/2
Result: 1.5707963267948966
> sin + 6
Result: 7.0
> + 9 - 5
Result: 11.0
> 4 + 6
Result: 10.0
```

Here we replace the last result with the evaluation of the expression.

#### Implicit Evaluation
As a result of the last point, inputting `0 + expr` is the same as first selecting the `=` option and then evaluating `expr`

Example:
```text
> 0 + (3 + 5)
Result: 8.0
> =
Enter an expression to evaluate:
3 + 5
Result: 8.0
```
### Important
Every operation evaluation that modifies the last result, will be checked first and then if the evaluation succeeds it will override the last result. If not, the last result won't change at all:

Example:
```text
> =
Enter an expression to evaluate:
-4
Result: -4.0
> sqrt
Error: CalcError {errorType = NegativeSqrt, expression = sqrt(-4.0)}
"Unknown command. Type 'help' for a list of commands."
> +0
Result: -4.0
```

---

## Supported Operations

### Constants
- `π` or `pi`: The mathematical constant Pi.
- `e`: Euler's number.

### Unary Operations
- Trigonometric: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
- Hyperbolic: `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`
- Logarithmic: `ln`, `log10`
- Others: `sqrt`, `cbrt`, `deg`, `rad`, `1/x`, `x^2`, `x^3`, `!` (factorial)

### Binary Operations
- Arithmetic: `+`, `-`, `*`, `/`
- Exponentiation: `^` (power)
- Logarithmic: `Lny` (logarithm with a custom base)
- Scientific notation: `EE` (e.g., `x EE y` evaluates to `x * 10^y`)

---

## Error Handling

The calculator implements robust error handling for invalid operations or inputs. The following errors may be encountered:

| Error Type         | Description                                    |
|--------------------|------------------------------------------------|
| `DivisionByZero`   | Division by zero.                             |
| `NegativeSqrt`     | Square root of a negative number.             |
| `LogNonPositive`   | Logarithm of zero or a negative number.       |
| `InvalidLogBase`   | Invalid logarithm base (≤ 0 or base = 1).     |
| `DomainError`      | Input outside the valid domain (e.g., `asin(2)`). |
| `InvalidFactorial` | Factorial of a negative or non-integer value. |

### Examples:
1. **Division by Zero**
   ```text
   > 4 / 0
   Error: CalcError {errorType = DivisionByZero, expression = (4 / 0)}
   ```

2. **Negative Square Root**
   ```text
   > sqrt(-4)
   Error: CalcError {errorType = NegativeSqrt, expression = sqrt(-4.0)}
   ```

3. **Invalid Factorial**
   ```text
   > 3.5!
   Error: CalcError {errorType = InvalidFactorial, expression = (3.5)!}
   ```

### Special Cases

Some evaluations may result in `Infinity` or `NaN` due to the floating point limitations (really large computations for example), in any of these cases the last result won't be overrided, since the calculator would be in an inconsistent state. 

Examples:
```text
> 6 + 5
Result: 11.0
> =
Enter an expression to evaluate:
e ^ 10000000000
Result: Infinity
> +0
Result: 11.0
```

```text
> 6 + 5
Result: 11.0
> =
Enter an expression to evaluate:
e ^ 10000000000 - e ^ 10000000000
Result: NaN
> +0
Result: 11.0
```

---

## Unit Testing and Property-Based Testing

### Unit Tests
The project includes unit tests to validate:
1. **Parser Functionality:** Ensures correct parsing of valid and invalid expressions.
2. **Evaluation Accuracy:** Verifies the results of various mathematical operations.
3. **Memory Operations:** Tests the memory manipulation commands.

Example Unit Tests:
- Parsing `4 + 5` produces `(4 + 5)`.
- Evaluating `sin(π/2)` produces `1.0`.

### Property-Based Testing
Property-based tests using **Hedgehog** ensure correctness over random inputs. Random mathematical expressions are generated, evaluated, and their results compared against expected values. These tests cover:
1. Valid expressions for non-restricted operations.
2. Consistency between parsing and evaluation.

Examples of properties tested:
1. Evaluating `sin(π/2)` equals `1.0`.
2. Randomly generating valid expressions and verifying results within a precision threshold.

### Running Tests
To run both unit and property-based tests:
```bash
stack test
```

---

## Help Command
Use the `help` command to display a full list of supported commands, operations, and examples:
```text
> help
Scientific Calculator Help

Supported Commands:
  =       : Evaluate an expression.
  ac      : Clear the last result (sets it to 0.0).
  m+      : Add the last result to memory.
  m-      : Subtract the last result from memory.
  mc      : Clear the memory (sets it to 0.0).
  mr      : Retrieve the value in memory.
  help    : Show this help message.
  quit/q  : Exit the calculator.
...
```

--- 
This calculator ensures accuracy, robustness, and flexibility for scientific computations.
