# sudoku_haskell

This is an implementation of the Sudoku game in Haskell.  
Final project in CS 557 Functional Languages at Portland State University.

Authors: Cosimo Gonnelli & Jane Seigman

## Setup & Launch

```stack build```  
```stack exec sudoku```  

### Gameplay & Sudoku Rules

Sudoku rules as stated by the wikipedia page:  
"The objective is to fill a 9×9 grid with digits so that each column, each row, and each of the nine 3×3 subgrids that compose the grid (also called "boxes", "blocks", or "regions") contain all of the digits from 1 to 9. The starting board provides a partially completed grid, which for a well-posed puzzle has a single solution."

Our game gives you 9 cookies at the beginning but if you make a mistake by entering the wrong number at an index, you will lose a cookie.  
If you lose all the cookies, you lose the game! Fill in all the numbers correctly and you will win the game!

This implementation maps the 0..8 coordinates to 1..9 to make it more user friendly  
For example, to play at coordinate (C0, C0), the user would enter ```1 1```

To enter a number, use this format: (coordinate x) (coordinate y) number   
For example, to input the number 4 at index (1, 3) you would enter: ```1 3 4``` 

To ask for help, Use this format: (coordinate x) (coordinate y)  
For example, to ask for a hint at index (1, 3), you would enter: ```1 3```  
If the index entered is empty, the hint function will fill in the correct number on the board.

## Resources

This implementation was built upon the TicTacToe codebase provided in CS 557 Functional Languages homework 1  
which was authored by the instructor Katie Casemento.

Sample play board in .txt files are from https://en.wikipedia.org/wiki/Sudoku

## About the Development

We used the Haskell Tool Stack to build the project: https://docs.haskellstack.org/en/stable/README/

#### Board Logistics

The board is comprised of 81 cells in a 9 x 9 grid. Each cell is an index which is a pair of coordinates C0..C8.  

The board is sectioned into 9 rows, 9 columns, and 9 blocks that are 9 cells each.  

#### Solve Functions

We started off by creating several functions for solving the board, 3 for checking rows,  
3 for checking columns, and 3 for checking blocks.    
In order to create a solution function that was generic, we created rowIndices, columnIndices,  
and blockIndices that are all of type ```[[Index]]```  

We came up with two different implementations to solve the board, one using list comprehensions and one using set-theoretic reasoning over lists.    
In order to try out both, alter the solve function to uncomment one or the other.    

#### I/O Functionality

We wrote two functions that handle gameplay: play and playerAct      
The play function checks that the board is still in-progress and win/lose conditions  
and calls the playerAct function passing in a copy of the new updated board and cookies value.    
The playerAct function handles both types of user input: playing a number to the board or asking for a hint.    

Main reads in 2 .txt files, a starting board (start_board.txt) and its solution (solved_board.txt), and calls the play function.    
