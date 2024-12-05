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

The application consists of an interpreter, a parser, and an evaluator.
- Since the task requires both IO and a memory state (variables), the interpreter operates on a `REPL = StateT (Map String Double) IO` monad transformer. The interpreter isolates effects from both monads, so the parser and the evaluator don't have to deal with them. It also collects errors from the parser and interpreter.
- The parser produces an AST from text input. It uses `buildExpressionParser` from Parsec, so the implementation is quite simple. Therefore, I didn't feel the need to separate it from the lexer.
- The evaluator takes an AST and produces the arithmetic result. The regular nature of binary operations allowed me to reuse much of the code for generic arithmetic operations. Most of the code was taken from my solution of HW04.

#### Project structure
- Main
    - `main :: IO ()` - runs the interpreter
- Interpreter
    - `runInterpreter :: REPL ()` - reads string instructions in an infinite loop, feeds them into `evalCommand` (unless :quit)
    - `evalCommand :: String -> REPL ()`
        - If got command, evaluate it on the spot
        - If got expression, pass it to the parser and evaluator, `ans = result`
        - Report errors from the parser
- Parser
    - `parse :: String -> Either ParseError Expr`
- Eval
    - `evalExpr :: CalcState -> Expr -> Either ArithmeticError Double`
- Expr
    - `Expr` - AST
    - `EvalError` - arithmetic/evaluation error, with information on where it occurred
- Spec
    - Property&unit tests for parser and evaluator
- Benchmark
    - Complex expression generation; benchmarks for parser and evaluator

## Libraries

#### Parsec
Parsec has extensive documentation and large community base. It also has necessary tools specifically for parsing arithmetic expressions.
Speaking about performance, Parsec is comparatively slow (https://gitlab.com/FinnBender/haskell-parsing-benchmarks).
But as we will see later, it's still fast enough - any equation of reasonable size is parsed in less than 5 ms.

#### Tasty+Hedgehog
Used in the course => I'm already familiar with them; they also support unit- and property-based testing.
hedgehog - good Hackage rating (2.75).

## Benchmarks

```shell
benchmarking Parse Benchmarks/parseExpr, complexity 20
time                 329.5 μs   (323.7 μs .. 336.0 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 332.1 μs   (325.9 μs .. 338.9 μs)
std dev              22.44 μs   (17.29 μs .. 32.64 μs)
variance introduced by outliers: 62% (severely inflated)
                                   
benchmarking Parse Benchmarks/parseExpr, complexity 40
time                 725.0 μs   (715.3 μs .. 735.3 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 734.6 μs   (727.6 μs .. 747.0 μs)
std dev              35.35 μs   (27.33 μs .. 45.79 μs)
variance introduced by outliers: 40% (moderately inflated)
                                   
benchmarking Parse Benchmarks/parseExpr, complexity 80
time                 1.359 ms   (1.235 ms .. 1.483 ms)
                     0.960 R²   (0.948 R² .. 0.988 R²)
mean                 1.241 ms   (1.200 ms .. 1.288 ms)
std dev              153.8 μs   (112.2 μs .. 205.7 μs)
variance introduced by outliers: 79% (severely inflated)
                                   
benchmarking Parse Benchmarks/parseExpr, complexity 160
time                 3.153 ms   (2.627 ms .. 3.574 ms)
                     0.911 R²   (0.865 R² .. 0.959 R²)
mean                 2.758 ms   (2.627 ms .. 2.918 ms)
std dev              506.0 μs   (428.3 μs .. 703.7 μs)
variance introduced by outliers: 88% (severely inflated)
                                   
benchmarking Eval Benchmarks/evalExpr, complexity 20
time                 421.3 ns   (408.0 ns .. 437.0 ns)
                     0.988 R²   (0.981 R² .. 0.993 R²)
mean                 458.0 ns   (432.0 ns .. 490.3 ns)
std dev              98.63 ns   (59.97 ns .. 138.2 ns)
variance introduced by outliers: 98% (severely inflated)
                                   
benchmarking Eval Benchmarks/evalExpr, complexity 40
time                 927.7 ns   (835.3 ns .. 1.017 μs)
                     0.912 R²   (0.878 R² .. 0.951 R²)
mean                 1.065 μs   (979.2 ns .. 1.193 μs)
std dev              363.1 ns   (290.0 ns .. 470.5 ns)
variance introduced by outliers: 99% (severely inflated)
                                   
benchmarking Eval Benchmarks/evalExpr, complexity 80
time                 1.413 μs   (1.288 μs .. 1.587 μs)
                     0.880 R²   (0.826 R² .. 0.937 R²)
mean                 1.850 μs   (1.686 μs .. 2.159 μs)
std dev              728.4 ns   (542.0 ns .. 969.8 ns)
variance introduced by outliers: 99% (severely inflated)
                                   
benchmarking Eval Benchmarks/evalExpr, complexity 160
time                 152.9 ns   (146.0 ns .. 163.8 ns)
                     0.923 R²   (0.876 R² .. 0.958 R²)
mean                 204.5 ns   (183.1 ns .. 226.4 ns)
std dev              80.09 ns   (64.94 ns .. 100.2 ns)
variance introduced by outliers: 100% (severely inflated)
```
Expressions of varying complexity were generated. For every generated `expr`, `(parseExpr . show) expr` and `evalExpr varMap expr` were run.
Results show that parsing speed is approximately linear from the size of input (which is expected), while evaluation speed is highly unpredictable (which can be seen by variance introduced by outliers).
Anyway, parsing and evaluation speed are approximately the same order of magnitude, with parsing generally being more costly.
This is unexpected; I expected arithmetic operations to take more time. Maybe if more computationally expensive functions, like trigonometric ones, were introduced, this would change.
