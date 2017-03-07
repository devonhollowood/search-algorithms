module Algorithm.SearchSpec (
  main,
  spec
  ) where

import Test.Hspec
import Algorithm.Search
import qualified Data.Map as Map

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
cyclicWeightedGraph :: Map.Map Char [(Int, Char)]
cyclicWeightedGraph = Map.fromList [
  ('a', [(1, 'b'), (2, 'c')]),
  ('b', [(1, 'a'), (2, 'c'), (5, 'd')]),
  ('c', [(1, 'a'), (2, 'd')]),
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
      bfs (cyclicUnweightedGraph Map.!) (== 4) [] 0
        `shouldBe` Just [1, 4]
    it "handles pruning" $
      bfs (cyclicUnweightedGraph Map.!) (== 4) [odd] 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      bfs (cyclicUnweightedGraph Map.!) (== 4) [odd, (== 6)] 0
        `shouldBe` Nothing
  describe "dfs" $ do
    it "performs depth-first search" $
      dfs (cyclicUnweightedGraph Map.!) (== 4) [] 0
        `shouldBe` Just [3, 2, 8, 5, 4]
    it "handles pruning" $
      dfs (cyclicUnweightedGraph Map.!) (== 4) [odd] 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $
      dfs (cyclicUnweightedGraph Map.!) (== 4) [odd, (== 6)] 0
        `shouldBe` Nothing
    it "handles doubly-inserted nodes" $
      dfs (acyclicUnweightedGraph Map.!) (==4) [] 0
        `shouldBe` Just [1, 4]
  describe "dijkstra" $ do
    it "performs dijkstra's algorithm" $
      dijkstra (cyclicWeightedGraph Map.!) (== 'd') [] 'a'
        `shouldBe` Just (4, [(2, 'c'), (2, 'd')])
    it "handles pruning" $
      dijkstra (cyclicWeightedGraph Map.!) (== 'd') [(== 'c')] 'a'
        `shouldBe` Just (6, [(1, 'b'), (5, 'd')])
    it "returns Nothing when no path is possible" $
      dijkstra (cyclicWeightedGraph Map.!) (== 'd') [(== 'b'), (== 'c')] 'a'
        `shouldBe` Nothing
    it "handles zero-length solutions" $
      dijkstra (cyclicWeightedGraph Map.!) (== 'd') [] 'd'
        `shouldBe` Just (0, [])
  describe "aStar" $ do
    let start = (0, 0)
        end = (2, 0)
        next =
          map (\pt -> (1, taxicabDistance pt end, pt))
          . taxicabNeighbors
    it "performs the A* algorithm" $
      aStar next (== end) [] start
        `shouldBe` Just (2, [(1, (1, 0)), (1, (2, 0))])
    it "handles pruning" $
      aStar next (== end) [isWall] start
        `shouldBe` Just (6, [(1, (0, 1)),
                             (1, (0, 2)),
                             (1, (1, 2)),
                             (1, (2, 2)),
                             (1, (2, 1)),
                             (1, (2, 0))
                            ])
    it "returns Nothing when no path is possible" $
      aStar next (== end) [isWall, \ p -> taxicabDistance p (0,0) > 1] start
        `shouldBe` Nothing
