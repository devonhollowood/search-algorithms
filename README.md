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

## Using A* to find edit distance:
```haskell
import Algorithm.Search (astar)

edits :: String -> [String]
edits str = replacements str ++ additions str ++ subtractions str
  where
    replacements [] = []
    replacements (c : cs) =
      map (: cs) ['a' .. 'z'] ++ map (c :) (replacements cs)
    additions [] = map (: []) ['a' .. 'z']
    additions (c : cs) = map (: c : cs) ['a' .. 'z'] ++ map (c :) (additions cs)
    subtractions [] = []
    subtractions (c : cs) = cs : map (c :) (subtractions cs)

lowerBoundEditDist :: String -> String -> Int
lowerBoundEditDist a "" = length a
lowerBoundEditDist "" b = length b
lowerBoundEditDist (a : as) (b : bs) =
  (if a == b then 0 else 1) + lowerBoundEditDist as bs

editDist from to = aStar next (== to) [] from
  where
    next = map (\str -> (1, lowerBoundEditDist str "frog", str)) . edits

-- editDist gives the edit distance between two strings, and the incremental
-- costs and strings along the way:
-- >>> editDist "dog" "frog"
-- Just (2, [(1, "drog"), (1, "frog")])
```
