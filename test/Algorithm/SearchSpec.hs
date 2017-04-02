module Algorithm.SearchSpec (
  main,
  spec
  ) where

import Test.Hspec
import Algorithm.Search
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = hspec spec

-- | Example cyclic directed unweighted graph
cyclicUnweightedGraph :: Map.Map Int [Int]
cyclicUnweightedGraph = Map.fromList [
  (0, [1, 2, 3]),
  (1, [4, 6]),
  (2, [0, 1, 6, 8]),
  (3, [1, 2]),
  (4, [0]),
  (5, [4]),
  (6, [4]),
  (8, [0, 5])
  ]

-- | Example acyclic directed unweighted graph
acyclicUnweightedGraph :: Map.Map Int [Int]
acyclicUnweightedGraph = Map.fromList [
  (0, [1, 2, 3]),
  (1, [4]),
  (2, [5]),
  (3, [2]),
  (4, []),
  (5, [])
  ]

-- | Example cyclic directed weighted graph
cyclicWeightedGraph :: Map.Map Char [(Char, Int)]
cyclicWeightedGraph = Map.fromList [
  ('a', [('b', 1), ('c', 2)]),
  ('b', [('a', 1), ('c', 2), ('d', 5)]),
  ('c', [('a', 1), ('d', 2)]),
  ('d', [])
  ]

-- | Example for taxicab path finding
taxicabNeighbors :: (Int, Int) -> [(Int, Int)]
taxicabNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]

isWall :: (Int, Int) -> Bool
isWall(x,y)=x==1&&(-2)<=y&&y<=1

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

spec :: Spec
spec = do
  describe "bfs" $ do
    it "performs breadth-first search" $
      bfs (cyclicUnweightedGraph Map.!) [] (== 4) 0
        `shouldBe` Just [1, 4]
    it "handles pruning" $
      bfs (cyclicUnweightedGraph Map.!) [odd] (== 4) 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      bfs (cyclicUnweightedGraph Map.!) [odd, (== 6)] (== 4) 0
        `shouldBe` Nothing
  describe "dfs" $ do
    it "performs depth-first search" $
      dfs (cyclicUnweightedGraph Map.!) [] (== 4) 0
        `shouldBe` Just [3, 2, 8, 5, 4]
    it "handles pruning" $
      dfs (cyclicUnweightedGraph Map.!) [odd] (== 4) 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      dfs (cyclicUnweightedGraph Map.!) [odd, (== 6)] (== 4) 0
        `shouldBe` Nothing
    it "handles doubly-inserted nodes" $
      dfs (acyclicUnweightedGraph Map.!) [] (==4) 0
        `shouldBe` Just [1, 4]
  describe "dijkstra" $ do
    let next = map fst . (cyclicWeightedGraph Map.!)
        cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
    it "performs dijkstra's algorithm" $
      dijkstra next cost [] (== 'd') 'a'
        `shouldBe` Just (4, ['c', 'd'])
    it "handles pruning" $
      dijkstra next cost [(== 'c')] (== 'd') 'a'
        `shouldBe` Just (6, ['b', 'd'])
    it "returns Nothing when no path is possible" $
      dijkstra next cost [(== 'b'), (== 'c')] (== 'd') 'a'
        `shouldBe` Nothing
    it "handles zero-length solutions" $
      dijkstra next cost [] (== 'd') 'd'
        `shouldBe` Just (0, [])
  describe "aStar" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs the A* algorithm" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end) [] (== end)
        start
        `shouldBe` Just (2, [(1, 0), (2, 0)])
    it "handles pruning" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end) [isWall]
        (== end) start
        `shouldBe` Just (6, [(0, 1), (0, 2), (1, 2), (2, 2), (2, 1), (2, 0)])
    it "returns Nothing when no path is possible" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end)
        [isWall, \ p -> taxicabDistance p (0,0) > 1] (== end) start
        `shouldBe` Nothing
    it "handles zero-length solutions" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end) []
        (== start) start
        `shouldBe` Just (0, [])
  describe "incrementalCosts" $ do
    let cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
    it "gives the incremental costs along a path" $
      incrementalCosts cost ['a', 'b', 'd'] `shouldBe` [1, 5]
    it "handles zero-length paths" $ do
      incrementalCosts cost [] `shouldBe` []
      incrementalCosts cost ['a'] `shouldBe` []
