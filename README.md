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

My project was Reversi Game, all of the criterias are written above. For me first task was to understand what is this game about, what are the rules and who wins. Most of the knowledge I got from the first website that pops up searching Reversi game https://cardgames.io/reversi/

I played more than 20 games with the bot(I never won the game 😢). Basically you are starting in n x n(n is even) board with 4 pieces at the center, 2 yours and 2 enemy pieces. You must try to capture opponent pieces and flip them over so they turn into your color. You do this by making a horizontal, vertical or diagonal line of pieces, where your pieces surround the other player's pieces. The surrounded opponent pieces are then captured and will be flipped over to your color, increasing the number of your pieces on the board. All same aplies to your enemy. The goal is to get as much pieces as possible, the one with the most wins.

## Libraries

I have 2 Libaries- Array and Gloss. Array is used to represent board of the game. For the Gloss, I was searching the internet for some reference on how people create games via Haskell and for me Gloss was really helpful. It has helpful Datas(Picture, Color, Display), Interfaces(I used play and display for finding bugs). 

## Architecture 

I splitted my project into 4 files

### Main

It waits for the 3 Integers: size of the board, screen width and screen height. It checks whether all entered correctly and runs the play function.
### Game

Here I defined all essential datas for the Game. The most essential one is Data Game, which consists of the gameBoard(the state of the board), player(who's turn is it), state(the state of the game, Running or Finished), size of the board and screen size(width and height). Also it has other supportive datas. 

### Rendering

This module is repsonsible for translating the picture to the program. The most important function is gameAsPicture that is used in play function. For translation of the game I separated it into 3 layers: 

BoardGrid - shows the grid of a Reversi game.

PlayerOneBoard - shows all of the pieces of first player.

PlayerTwoBoard - shows all of the pieces of second player.

### Logic

This module is responsible for all logic of the game, it checks whether the move is right, is there any move at all, who won the game, deals with consequences of the move(changing the opposing player's pieces) and etc. The important function is transformGame which is used in play function. It listens to the mouse Left Button and changes state of the game according to it.

## Investigation

### (20 points) Console (or other user) interface.

I use Gloss play function that initializes a windows with the game interface. It supports interaction with this window via mouse left button.

### (20 points) Implement game rules, including valid moves and flipping discs.
All of the valid moves are implemented in the game and its consequences such as flipping enemy discs, rules are followed. 

### (10 points) Support for two human players.
The games allows two human interaction.

### (10 points) Allow different board sizes for varied gameplay.
The game required to be configured at the beginning of it with 3 parameters: size of the board, screen width and screen height.
### (10 points) Add undo and redo functionality.

### (15 points) Error reporting for invalid moves.
All of the invalid moves are not interactable.
### (15 points) Unit and property-based tests.
