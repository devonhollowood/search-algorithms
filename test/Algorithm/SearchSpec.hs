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
cyclic_unweighted_graph = Map.fromList [
  (0, [1, 2, 3]),
  (1, [4]),
  (2, [0, 1, 6, 8]),
  (3, [1, 2]),
  (4, [0]),
  (5, [4]),
  (6, [4]),
  (8, [0, 5])
  ]

-- | Example cyclic directed weighted graph
cyclic_weighted_graph = Map.fromList [
  ('a', [(1, 'b'), (2, 'c')]),
  ('b', [(1, 'a'), (2, 'c'), (5, 'd')]),
  ('c', [(1, 'a'), (2, 'd')]),
  ('d', [])
  ]

spec :: Spec
spec = do
  describe "bfs" $ do
    it "performs breadth-first search" $ do
      bfs (cyclic_unweighted_graph Map.!) (== 4) [] 0
        `shouldBe` Just [1, 4]
    it "handles pruning" $ do
      bfs (cyclic_unweighted_graph Map.!) (== 4) [odd] 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $ do
      bfs (cyclic_unweighted_graph Map.!) (== 4) [odd, (== 6)] 0
        `shouldBe` Nothing
  describe "dfs" $ do
    it "performs depth-first search" $ do
      dfs (cyclic_unweighted_graph Map.!) (== 4) [] 0
        `shouldBe` Just [2, 8, 5, 4]
    it "handles pruning" $ do
      dfs (cyclic_unweighted_graph Map.!) (== 4) [odd] 0
        `shouldBe` Just [2, 6, 4]
    it "returns Nothing when no path is possible" $ do
      dfs (cyclic_unweighted_graph Map.!) (== 4) [odd, (== 6)] 0
        `shouldBe` Nothing
  describe "dijkstra" $ do
    it "performs dijkstra's algorithm" $ do
      dijkstra (cyclic_weighted_graph Map.!) (== 'd') [] 'a'
        `shouldBe` Just (4, [(2, 'c'), (2, 'd')])
    it "handles pruning" $ do
      dijkstra (cyclic_weighted_graph Map.!) (== 'd') [(== 'c')] 'a'
        `shouldBe` Just (6, [(1, 'b'), (5, 'd')])
    it "returns Nothing when no path is possible" $ do
      dijkstra (cyclic_weighted_graph Map.!) (== 'd') [(== 'b'), (== 'c')] 'a'
        `shouldBe` Nothing
