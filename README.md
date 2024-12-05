# Projects 

## Deadline 23:59 5.12.2024

A project amounts to 20 points of your classwork grade (max is 50). 
A project is to be done by a single student, no teamwork allowed. 
The maximum number of points for a project is 100, with concrete subtasks having different values.
Each project should be followed by a written report in which the following topics are discussed:

* What the task was.
* The architecture of your solution.
* Why certain architecture decisions were done. 
* Why certain libraries were chosen.
* Investigation of the performance.

Choose your project [here](https://docs.google.com/spreadsheets/d/138z6qgMBtROwDxh4niDajSIGZNb4r-ucpLowJDk7qjM/edit?usp=sharing). Max 3 students can take the same project, but they should work on it independently. 

## Reversi Game

* (20 points) Console (or other user) interface.
* (20 points) Implement game rules, including valid moves and flipping discs.
* (10 points) Support for two human players.
* (10 points) Allow different board sizes for varied gameplay.
* (10 points) Add undo and redo functionality.
* (15 points) Error reporting for invalid moves.
* (15 points) Unit and property-based tests.

## How to run the Game
Go to ReversiFinal directory, build and run the cabal.
```
$ cd ReversiFinal
$ cabal build
$ cabal run
```

## What the task was

My project was the Reversi Game. All of the criteria are written above. For me, the first task was to understand what this game is about, what the rules are, and who wins. Most of the knowledge I got was from the first website that pops up when searching for the Reversi game: [https://cardgames.io/reversi/](https://cardgames.io/reversi/).

I played more than 20 games with the bot (I never won the game 😢). Basically, you start on an `n x n` (where `n` is even) board with 4 pieces at the center: 2 of yours and 2 enemy pieces. You must try to capture opponent pieces and flip them over so they turn into your color. You do this by making a horizontal, vertical, or diagonal line of pieces where your pieces surround the other player's pieces. The surrounded opponent pieces are then captured and will be flipped over to your color, increasing the number of your pieces on the board. The same applies to your enemy. The goal is to get as many pieces as possible; the one with the most wins.

## Libraries

I have 2 libraries: Array and Gloss. Array is used to represent the board of the game. For Gloss, I was searching the internet for some references on how people create games in Haskell, and for me, Gloss was really helpful. It has helpful data types (`Picture`, `Color`, `Display`) and interfaces (I used `play` and `display` for finding bugs). Overall Gloss made the process much easier and understandable.

Also I used tasty for the tests.

## Architecture 

I split my project into 4 files:

### Main

It waits for 3 integers: the size of the board, screen width, and screen height. It checks whether all inputs are correct and then runs the `play` function.

### Game

Here I defined all essential data for the game. The most essential one is the `Game` data, which consists of the `gameBoard` (the state of the board), `player` (whose turn it is), `state` (the state of the game: Running or Finished), the size of the board, and screen dimensions (width and height). It also has other supportive data types. 

### Rendering

This module is responsible for translating the picture to the program. The most important function is `gameAsPicture`, which is used in the `play` function. For translating the game, I separated it into 3 layers:

- **BoardGrid**: Displays the grid of a Reversi game.
- **PlayerOneBoard**: Shows all the pieces of the first player.
- **PlayerTwoBoard**: Shows all the pieces of the second player.

### Logic

This module is responsible for all the logic of the game. It checks whether the move is valid, whether there are any moves available, determines who won the game, and deals with the consequences of moves (e.g., flipping the opposing player's pieces). The important function is `transformGame`, which is used in the `play` function. It listens to the mouse left button and changes the state of the game accordingly.

## Investigation

I did not find any bugs in game process, everything runs smoothly and according to the game rules. In terms of efficiency there is definetely room for improvement in terms of cleaning the look of the code and performance of it, one of the example would be the logic module, where I could have get rid of repeticience in the code, but it was too early for me to notice it. Also I am not very fond of the Game Data, as previously I didn't have screenWidth, screenHeight and n variables in it, but I couldn't come up with better solution for the one of the subtasks(allow different board sizes for varied gameplay).

### (20 points) Console (or other user) interface.

I use the Gloss `play` function, which initializes a window with the game interface. It supports interaction with this window via the mouse left button.

### (20 points) Implement game rules, including valid moves and flipping discs.

All of the valid moves are implemented in the game along with their consequences, such as flipping enemy discs. The rules are followed. 

### (10 points) Support for two human players.

The game allows interaction between two human players.

### (10 points) Allow different board sizes for varied gameplay.

The game is configured at the beginning with 3 parameters: the size of the board, screen width, and screen height.

### (10 points) Add undo and redo functionality.

Not implemented, but I have in mind how I would have done it. I would just add another listener on u and r buttons. Also I would have added 2 instances past and future, that would have been a cell. It would have remembered the previous move and future(If we did undo).
### (15 points) Error reporting for invalid moves.

All invalid moves are non-interactable, there is no need for it. There is need for reporting on invalid inputs of the board size, screen width and height, which I implemented.

### (15 points) Unit and property-based tests.
Created basic tests.