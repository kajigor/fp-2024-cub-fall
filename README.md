# midterm

## Deadline: 09:35 30.10.2024

The midterm consists of two parts: a test and a coding assignment. You have 75 minutes to complete both, and the order of submission does not matter. Anything submitted after the deadline will not be checked. If your code does not compile, it will not be checked. 

While working on the midterm, you are allowed to use ghci, ghc, hoogle, and vscode extension. You must work on your own. Cheating of any kind, including copying code of your classmates is not permitted: this is against the Code of Conduct of the university.

## Test

Follow the link in the [table](https://docs.google.com/spreadsheets/d/1B1rBfjwMu3NL8Ssa6BptbRfpvG7mRZUlvfs9NTQyeEc/edit?usp=sharing) to get access to your midterm variant. 

## Coding assignment

Your task is to create a data structure that reflects the hierarchy of directories and files in the given file. This data structure can then be used for various manipulations, but we will limit ourselves with printing it to a string. 

Your code will be graded based not only on whether it functions or not, but also on the use of appropriate Haskell features, library functions, and its readability. Make sure to avoid code duplication and split your code into modules in a way it makes sense. You are expected to add necessary dependencies on your own. You can derive instances, but not edit types of the given functions or data types declarations. 

1. [1 point] In [midterm/src/Directory.hs](midterm/src/Directory.hs), define an appropriate type called `DirectoryTree` to represent the directory hierarchy based on the data structure defined in the [midterm/src/Tree.hs](midterm/src/Tree.hs).

2. [10 points] In [midterm/src/Directory.hs](midterm/src/Directory.hs), implement a function `buildTree :: FilePath -> IO DirectoryTree` that creates a tree which represents the hierarchy of files and subdirectories of the given directory.
  
   * Ignore symlinks and other complications. 
   * Hoogle functions such as `doesDirectoryExist` and `listDirectory` and use them.

3. [5 points] In [midterm/src/Directory.hs](midterm/src/Directory.hs), implement a function that displays the `DirectoryTree`. The subdirectories of the current directory should go before the files in it. Example of use on the [midterm](midterm) directory: 

```
/Users/.../midterm
  .stack-work
    dist
    ghci
    install
    stack.sqlite3
    stack.sqlite3.pantry-write-lock
  app
    Main.hs
  src
    Directory.hs
    Tree.hs
  test
    Spec.hs
  .gitignore
  CHANGELOG.md
  LICENSE
  README.md
  Setup.hs
  midterm.cabal
  package.yaml
  stack.yaml
  stack.yaml.lock
```

4. [5 points] In [midterm/src/Directory.hs](midterm/src/Directory.hs), implement a function that displays the `DirectoryTree`. The subdirectories and the files within the current directory should be ordered alphabetically.  Example of use on the [midterm](midterm) directory: 

```
/Users/.../midterm
  .gitignore
  .stack-work
    dist
    ghci
    install
    stack.sqlite3
    stack.sqlite3.pantry-write-lock
  CHANGELOG.md
  LICENSE
  README.md
  Setup.hs
  app
    Main.hs
  midterm.cabal
  package.yaml
  src
    Directory.hs
    Tree.hs
  stack.yaml
  stack.yaml.lock
  test
    Spec.hs
```

5. [3 points] In [midterm/app/Main.hs](midterm/app/Main.hs), implement a function `run` that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.

6. [1 points] In [midterm/app/Main.hs](midterm/app/Main.hs), implement the `main` function that calls the `run` to demonstrate your code works. 
 