module Algorithm.Search (
  bfs,
  dfs,
  dijkstra
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

-- | Perform a breadth-first search over a set of states
--
-- Example: Making change problem
--
-- >>> :{
-- countChange target = bfs add_one_coin (== target) [(> target)] 0
--   where
--     add_one_coin amt = map (+ amt) coins
--     coins = [1, 5, 10, 25]
-- :}
--
-- >>> countChange 67
-- Just [1,2,7,17,42,67]
bfs :: Ord state =>
  (state -> [state])
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'bfs' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> [state -> Bool]
  -- ^ List of ways to prune search. These are predicates which, if 'True', are
  -- considered to indicate a "dead end".
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
bfs = search Seq.empty

-- | Perform a depth-first search over a set of states
--
-- Example: Simple directed graph search
--
-- >>> import qualified Data.Map as Map
--
-- >>> graph = Map.fromList [(1, [2, 3]), (2, [4]), (3, [4]), (4, [])]
--
-- >>> dfs (graph Map.!) (== 4) [] 1
-- Just [3,4]
dfs :: Ord state =>
  (state -> [state])
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dfs' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> [state -> Bool]
  -- ^ List of ways to prune search. These are predicates which, if 'True', are
  -- considered to indicate a "dead end".
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
dfs = search []

-- | Perform a shortest-path search over a set of states using Dijkstra's
-- | algorithm. Given a set of way of generating neighboring states and
-- | incremental costs from a current state, this will find the least-costly
-- | path from an initial state to a state matching a given predicate
--
-- Example: Making change problem, with a twist: you only have rare misprint
-- dimes which are really worth $10 each
--
-- >>> :{
-- countChange target = dijkstra add_one_coin (== target) [(> target)] 0
--   where
--     add_one_coin amt =
--       map (\(true_val, face_val) -> (true_val, face_val + amt)) coin_values
--     coin_values = [(1, 1), (5, 5), (1000, 10), (25, 25)]
-- :}
--
-- >>> countChange 67
-- Just (67,[(1,1),(1,2),(5,7),(5,12),(5,17),(25,42),(25,67)])
dijkstra :: (Ord state, Num cost, Ord cost) =>
  (state -> [(cost, state)])
  -- ^ Function to generate list of incremental cost and neighboring states
  -- given the current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dijkstra' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> [state -> Bool]
  -- ^ List of ways to prune search. These are predicates which, if 'True', are
  -- considered to indicate a "dead end".
  -> state
  -- ^ Initial state
  -> Maybe (cost, [(cost, state)])
  -- (Total cost, [(incremental cost, step)]) for the first path found which
dijkstra next solved prunes initial =
  go (Map.singleton initial (0, [])) Set.empty initial
  where
    go visited queue current
      | solved current = Just $ reverse <$> (visited Map.! current)
      | otherwise =
        let (old_cost, old_steps) = Maybe.fromJust $ Map.lookup current visited
            new_cost_steps =
              Map.fromList
              . map (\(incr, st) ->
                       let new_cost = old_cost + incr
                       in (st, (new_cost, (incr, st) : old_steps))
                    )
              $ next current
            new_visited = Map.unionWith less_costly visited new_cost_steps
            new_queue =
              List.foldl' push queue
              . map (\(st, (cost, _)) -> (cost, st))
              . filter (not . \(st, _) ->
                           st `Map.member` visited || any ($ st) prunes
                       )
               $ Map.toList new_cost_steps
        in pop new_queue >>= (\((_, st), queue') -> go new_visited queue' st)
    less_costly a b = if fst a <= fst b then a else b

search :: (Ord state, SearchContainer f) =>
  f state
  -- ^ empty 'SearchContainer'
  -> (state -> [state])
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'search' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> [state -> Bool]
  -- ^ List of ways to prune search. These are predicates which, if 'True', are
  -- considered to indicate a "dead end".
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
search empty next solved prunes initial =
  reverse <$> go (Map.singleton initial []) empty initial
  where
    go visited queue current
      | solved current = Just $ visited Map.! current
      | otherwise =
        let steps_so_far = Maybe.fromJust $ Map.lookup current visited
            new_states =
              Map.fromList . map (\st -> (st, st:steps_so_far)) $ next current
            new_visited = Map.union visited new_states
            new_queue =
              List.foldl' push queue .
               filter (not . \st ->
                         st `Map.member` visited || any ($ st) prunes
                      )
               $ Map.keys new_states
        in pop new_queue >>= (\(x, xs) -> go new_visited xs x)

class SearchContainer f where
  pop :: Ord a => f a -> Maybe (a, f a)
  push :: Ord a => f a -> a -> f a

instance SearchContainer Seq.Seq where
  pop queue =
    case Seq.viewl queue of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push queue a = queue Seq.|> a

instance SearchContainer [] where
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

instance SearchContainer Set.Set where
  pop = Set.minView
  push = flip Set.insert
