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
-- the ordering here is important--for dfs, last state will be visited first
taxicabNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x, y - 1), (x + 1, y)]

isWall :: (Int, Int) -> Bool
isWall (x, y) = x == 1 && ((-2) <= y && y <= 1)

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

taxicabNeighborsBounded :: (Int, Int) -> Maybe [(Int, Int)]
taxicabNeighborsBounded (x, y)
  | outOfBounds (x, y) = Nothing
  | otherwise = Just $ taxicabNeighbors (x, y)

taxicabDistanceBounded :: (Int, Int) -> (Int, Int) -> Maybe Int
taxicabDistanceBounded (x1, y1) (x2, y2)
  | outOfBounds (x1, y1) || outOfBounds (x2, y2) = Nothing
  | otherwise = Just $ taxicabDistance (x1, y1) (x2, y2)

outOfBounds :: (Int, Int) -> Bool
outOfBounds (x, y) = abs x + abs y > 10

isBigWall :: (Int, Int) -> Bool
isBigWall (x, y) = x == 1 && ((-10) <= y && y <= 10)

spec :: Spec
spec = do
  describe "bfs" $ do
    let next = (cyclicUnweightedGraph Map.!)
    it "performs breadth-first search" $
      bfs next (== 4) 0 `shouldBe` Just [1, 4]
    it "handles pruning" $
      bfs (next `pruning` odd) (== 4) 0 `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      bfs (next `pruning` odd `pruning` (== 6)) (== 4) 0 `shouldBe` Nothing
  describe "dfs" $ do
    let next = (cyclicUnweightedGraph Map.!)
    it "performs depth-first search" $
      dfs next (== 4) 0 `shouldBe` Just [3, 2, 8, 5, 4]
    it "handles pruning" $
      dfs (next `pruning` odd) (== 4) 0 `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      dfs (next `pruning` odd `pruning` (== 6)) (== 4) 0 `shouldBe` Nothing
    it "handles doubly-inserted nodes" $
      dfs (acyclicUnweightedGraph Map.!) (==4) 0 `shouldBe` Just [1, 4]
  describe "dijkstra" $ do
    let next = map fst . (cyclicWeightedGraph Map.!)
        cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
    it "performs dijkstra's algorithm" $
      dijkstra next cost (== 'd') 'a'
        `shouldBe` Just (4, ['c', 'd'])
    it "handles pruning" $
      dijkstra (next `pruning` (== 'c')) cost (== 'd') 'a'
        `shouldBe` Just (6, ['b', 'd'])
    it "returns Nothing when no path is possible" $
      dijkstra (next `pruning` (== 'b') `pruning` (== 'c')) cost (== 'd') 'a'
        `shouldBe` Nothing
    it "handles zero-length solutions" $
      dijkstra next cost (== 'd') 'd' `shouldBe` Just (0, [])
  describe "aStar" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs the A* algorithm" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end) (== end)
        start
        `shouldBe` Just (2, [(1, 0), (2, 0)])
    it "handles pruning" $
      aStar (taxicabNeighbors `pruning` isWall) taxicabDistance
        (taxicabDistance end) (== end) start
        `shouldBe` Just (6, [(0, 1), (0, 2), (1, 2), (2, 2), (2, 1), (2, 0)])
    it "returns Nothing when no path is possible" $
      aStar
        (taxicabNeighbors
          `pruning` isWall
          `pruning` (\ p -> taxicabDistance p (0,0) > 1)
        )
        taxicabDistance
        (taxicabDistance end)
        (== end)
        start
        `shouldBe` Nothing
    it "handles zero-length solutions" $
      aStar taxicabNeighbors taxicabDistance (taxicabDistance end) (== start)
        start
        `shouldBe` Just (0, [])
  describe "bfsM" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs monadic breadth-first search" $ do
      bfsM taxicabNeighborsBounded (return . (== end)) start
        `shouldBe` Just (Just [(1, 0), (2, 0)])
      bfsM
        (taxicabNeighborsBounded `pruningM` (return . isWall))
        (return . (== end))
        start
        `shouldBe` Just (Just [(0,1),(0,2),(1,2),(2,2),(2,1),(2,0)])
    it "handles cyclic graphs" $ do
      let nextM = return . map fst . (cyclicWeightedGraph Map.!)
      bfsM nextM (return . (== 'd')) 'a'
        `shouldBe` Just (Just ['b', 'd'])
    it "correctly handles monadic behavior" $ do
      bfsM
        (taxicabNeighborsBounded `pruningM` (return . isBigWall))
        (return . (== end))
        start
        `shouldBe` Nothing
      bfsM taxicabNeighborsBounded (const Nothing) start
        `shouldBe` Nothing
  describe "dfsM" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs monadic depth-first search" $
      dfsM taxicabNeighborsBounded (return . (== end)) start
        `shouldBe` Just (Just [(1, 0), (2, 0)])
    it "handles doubly-inserted nodes" $ do
      let nextM = return . (acyclicUnweightedGraph Map.!)
      dfsM nextM (return . (== 4)) 0 `shouldBe` Just (Just [1, 4])
    it "correctly handles monadic behavior" $ do
      dfsM
        (taxicabNeighborsBounded `pruningM` (return . isBigWall))
        (return . (== end))
        start
        `shouldBe` Nothing
      dfsM taxicabNeighborsBounded (const Nothing) start
        `shouldBe` Nothing
  describe "dijkstraM" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs monadic dijkstra's algorithm" $
      dijkstraM
        taxicabNeighborsBounded
        taxicabDistanceBounded
        (return . (== end))
        start
        `shouldBe` Just (Just (2, [(1, 0), (2, 0)]))
    it "handles cyclic graphs" $ do
      let nextM = return . map fst . (cyclicWeightedGraph Map.!)
          costM a b = lookup b $ cyclicWeightedGraph Map.! a
      dijkstraM nextM costM (return . (== 'd')) 'a'
        `shouldBe` Just (Just (4, ['c', 'd']))
      dijkstraM (nextM `pruningM` (return . (== 'c'))) costM
        (return . (== 'd')) 'a'
        `shouldBe` Just (Just (6, ['b', 'd']))
    it "handles zero-length solutions" $ do
      let nextM = return . map fst . (cyclicWeightedGraph Map.!)
          costM a b = lookup b $ cyclicWeightedGraph Map.! a
      dijkstraM nextM costM (return . (== 'd')) 'd'
        `shouldBe` Just (Just (0, []))
    it "correctly handles monadic behavior" $ do
      dijkstraM
        (taxicabNeighborsBounded `pruningM` (return . isBigWall))
        taxicabDistanceBounded
        (return . (== end))
        start
        `shouldBe` Nothing
      dijkstraM
        taxicabNeighborsBounded
        ((const . const) Nothing :: (Int, Int) -> (Int, Int) -> Maybe Int)
        (return . (== end))
        start
        `shouldBe` Nothing
      dijkstraM
        (taxicabNeighborsBounded `pruningM` (return . isBigWall))
        taxicabDistanceBounded
        (const Nothing)
        start
        `shouldBe` Nothing
  describe "aStarM" $ do
    let start = (0, 0)
        end = (2, 0)
    it "performs a monadic A* algorithm" $
      aStarM
        (taxicabNeighborsBounded `pruningM` (return . isWall))
        taxicabDistanceBounded
        (taxicabDistanceBounded end)
        (return . (== end))
        start
        `shouldBe`
        Just (Just (6, [(0, 1), (0, 2), (1, 2), (2, 2), (2, 1), (2, 0)]))
    it "handles zero-length solutions" $
      aStarM taxicabNeighborsBounded taxicabDistanceBounded
        (taxicabDistanceBounded end) (return . (== start)) start
        `shouldBe` Just (Just (0, []))
    it "correctly handles monadic behavior" $ do
      aStarM
        (taxicabNeighborsBounded `pruningM` (return . isBigWall))
        taxicabDistanceBounded
        (taxicabDistanceBounded end)
        (return . (== end))
        start
        `shouldBe` Nothing
      aStarM
        taxicabNeighborsBounded
        ((const . const) Nothing :: (Int, Int) -> (Int, Int) -> Maybe Int)
        (taxicabDistanceBounded end)
        (return . (== end))
        start
        `shouldBe` Nothing
      aStarM
        taxicabNeighborsBounded
        taxicabDistanceBounded
        (const Nothing)
        (return . (== end))
        start
        `shouldBe` Nothing
      aStarM
        taxicabNeighborsBounded
        taxicabDistanceBounded
        (taxicabDistanceBounded end)
        (const Nothing)
        start
        `shouldBe` Nothing
  describe "incrementalCosts" $ do
    let cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
    it "gives the incremental costs along a path" $
      incrementalCosts cost ['a', 'b', 'd'] `shouldBe` [1, 5]
    it "handles zero-length paths" $ do
      incrementalCosts cost [] `shouldBe` []
      incrementalCosts cost ['a'] `shouldBe` []
  describe "incrementalCostsM" $ do
    let costM a b = lookup b $ cyclicWeightedGraph Map.! a
    it "gives monadic incremental costs along a path" $
      incrementalCostsM costM ['a', 'b', 'd'] `shouldBe` Just [1, 5]
    it "handles zero-length paths" $ do
      incrementalCostsM costM [] `shouldBe` Just []
      incrementalCostsM costM ['a'] `shouldBe` Just []
    it "correctly handles monadic behavior" $
      incrementalCostsM costM ['a', 'd'] `shouldBe` Nothing
