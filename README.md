# search-algorithms
Haskell library containing common graph search algorithms

Lots of problems can be modeled as graphs, but oftentimes we don't want to make an actual graph structure to represent the problem. Perhaps the graph is too big (or infinite), or maybe making an explicit graph is just unwieldy for the problem at hand. That's where this library comes in: this is a collection of generalized search algorithms, so that you don't have to make the graphs explicit! Examples:

## Change-making problem
```haskell
import Algorithm.Search (bfs)

countChange target = bfs add_one_coin (== target) [(> target)] 0
  where
    add_one_coin amt = map (+ amt) coins
    coins = [1, 5, 10, 25]
    
-- countChange gives the subtotals along the way to the end:
-- >>> countChange 67
-- Just [1, 2, 7, 17, 42, 67]
```

## Simple directed acyclic graph:
```haskell
import Algorithm.Search (dfs)
import qualified Data.Map as Map

graph = Map.fromList [
  (1, [2, 3]),
  (2, [4]),
  (3, [4]), 
  (4, [])
  ]

-- Run dfs on the graph:
-- >>> dfs (graph Map.!) (== 4) [] 1
-- Just [3,4]
```
