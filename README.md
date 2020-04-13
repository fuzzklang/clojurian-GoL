# game-of-life
Conway's Game of Life written in Clojure

(Not optimized and quite slow)

# Requirements
Download from https://github.com/fuzzklang/clojurian-GoL.

Requires leiningen: https://leiningen.org/.

## Usage
From terminal:

`lein run`  
or `lein run <num generations>`
or `lein run <num generations> <p>`  
or `lein run <num generations> <columns> <rows>`
or `lein run <num generations> <p> <columns> <rows>`  

## Options
num generations: Number of generations to run game for.  
columns:		 Number of columns.  
rows:			 Number of rows.  
p: 				 Probability for any cell to be alive at start.

## Examples
```
lein run
lein run 200
lein run 100 50 25
```