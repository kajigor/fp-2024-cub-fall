# scientific-calculator


## Task

- (10 points) Console (or other user) interface in a REPL style.
- (20 points) Parser for algebraic expressions (you are encouraged to use a library).
- (40 points) Evaluation engine supporting arithmetic operations typical in a scientific calculator. Notice that there are memory buttons.
- (15 points) Error reporting for invalid inputs or calculations.
- (15 points) Unit and property-based tests.

## Usage

Run the interpreter:
```shell
stack build
stack run
```
Run tests:
```shell
stack test
```
Run the benchmark:
```shell
stack bench
```

## Interface

#### Expressions
- Enter an arithmetic expression to evaluate it. Currently supported operations are +, -, \*, /, \*\*, abs().
- Expressions may also include variables (more on this below).

#### Variables
- Currently, only the internal variables `ans` and `mem` are supported:
    - `ans` stores the result of the last successful computation; defaults to 0.
    - `mem` is the value currently stored in memory; defaults to 0.
- Variables can be accessed in expressions, with their names in quotes (e. g. `("mem" + 2) * "ans"`)

#### Commands
- If input begins with ':', it's treated as a command.
- Currently supported commands include:
    - `:help`   prints this message
    - `:ms`     `mem := ans` (stores the result of the last successful computation to memory)
    - `:mc`     `mem := 0` (clears the value stored in memory)
    - `:mr`     outputs `mem`
    - `:m+`     `mem += ans`
    - `:m-`     `mem -= ans`
    - `:quit`   exits the interpreter

## Architecture

#### Modules
- main
    - `main :: IO ()` - runs the interpreter
- interpreter
    - `runInterpreter :: REPL ()` - reads string instructions in an infinite loop, feeds them into eval (unless :quit)
    - `read :: IO String`, `print :: String -> IO ()`
    - `evalCommand :: String -> REPL ()`
        - If got command, evaluate it on the spot
        - If got expression, pass it to the parser and evaluator, `ans = result`
        - Report errors
- parser
    - `parse :: String -> Either ParseError Expr`
- expr
    - `evalExpr :: CalcState -> Expr -> Either ArithmeticError Double`
- test
    - Property&unit tests for: parser, evaluator; interpreter?
    - Performance tests
      TODO: add motivation for architecture decisions

#### Types
- `type REPL = StateT (Map String (Either ArithmeticError Double)) IO`
- `Expr = Number Double | Var String | [Binop] Expr Expr | [Unop] Expr`
- `Error = ParseError | ArithmeticError`

## Libraries

#### Megaparsec

Great error handling (can report multiple parse errors at once, reports error position and custom error types in general). It's one of the only parsing libraries supporting this.
Megaparsec is medium in terms of speed (faster than Parsec, but slower than Flatparse), but speed is not much of a consideration in our case since the performance bottleneck is mathematical computations, not expression parsing.

#### Tasty+Hedgehog

Used in the course => I'm already familiar with it; supports unit- and property-based testing.
hedgehog - good Hackage rating (2.75).
TODO: add comparison with other libraries
