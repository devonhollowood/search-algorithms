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

## Using A* to find a path in an area with a wall:
```haskell
import Algorithm.Search (aStar)

taxicabNeighbors :: (Int, Int) -> [(Int, Int)]
taxicabNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]

isWall :: (Int, Int) -> Bool
isWall (x, y) = x == 1 && (-2) <= y && y <= 1

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findPath :: (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, (Int, Int))])
findPath start end =
  let next =
        map (\pt -> (1, taxicabDistance pt end, pt))
        . taxicabNeighbors
  in aStar next (== end) [isWall] start

-- findPath p1 p2 finds a path between @p1@ and @p2@, avoiding the wall
-- >>> findPath (0, 0) (2, 0)
-- Just (6,[(1,(0,1)),(1,(0,2)),(1,(1,2)),(1,(2,2)),(1,(2,1)),(1,(2,0))])
--
-- This correctly goes up and around the wall
```
