module Algorithm.Search (
  bfs,
  dfs,
  dijkstra,
  aStar
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
bfs = search Seq.empty const


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
dfs = search [] const


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


-- | Performs A* search algorithm.
--
-- This algorithm is similar to Dijkstra's algorithm, but in addition to
-- supplying it a way to generate a list of next states, you supply it a
-- lower bound on the remaining cost.
--
-- Example: Path finding in taxicab geometry
--
-- >>> :{
-- neighbors (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
-- dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)
-- nextTowards dest pos = map (\p -> (1, dist p dest, p)) (neighbors pos)
-- :}
--
-- >>> aStar (nextTowards (0, 2)) (== (0, 2)) [(== (0, 1))] (0, 0)
-- Just (4,[(1,(-1,0)),(1,(-1,1)),(1,(-1,2)),(1,(0,2))])
aStar :: (Ord state, Num cost, Ord cost) =>
  (state -> [(cost, cost, state)])
  -- ^ Function which, when given the current state, produces a list whose
  -- elements are (incremental cost to reach neighboring state,
  -- lower bound on remaining cost from said state, said state).
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'aStar' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> [state -> Bool]
  -- ^ List of ways to prune search. These are predicates which, if 'True', are
  -- considered to indicate a "dead end".
  -> state
  -- ^ Initial state
  -> Maybe (cost, [(cost, state)])
  -- (Total cost, [(incremental cost, step)]) for the first path found which
aStar next found prunes initial =
  -- The idea of this implementation is that we can use the same machinery as
  -- Dijkstra's algorithm, by changing Dijsktra's cost function to be
  -- (incremental cost + lower bound remaining cost). We'd still like to be able
  -- to return the list of incremental costs, so we modify the internal state to
  -- be (incremental cost to state, state). Then at the end we undo this
  -- transformation
  unpack <$> dijkstra next' (found . snd) (map (. snd) prunes) (0, initial)
  where
    next' (_, st) = map pack (next st)
    pack (incr, est, new_st) = (incr + est, (incr, new_st))
    unpack (_, packed_states) =
      let unpacked_states = map snd packed_states
      in (sum (map fst unpacked_states), unpacked_states)


data SearchState f state = SearchState {
  current :: state,
  queue :: f state,
  visited :: Map.Map state [state]
  }


nextSearchState :: (SearchContainer f, Ord state) =>
  ([state] -> [state] -> [state])
  -> (state -> [state])
  -> [state -> Bool]
  -> SearchState f state
  -> Maybe (SearchState f state)
nextSearchState choose next prunes old =
  let steps_so_far = visited old Map.! current old
      new_states = next (current old)
      (new_queue, new_visited) =
        List.foldl' (
        \(queue_st, visited_st) new_st ->
          if not (new_st `Map.member` visited_st || any ($ new_st) prunes)
          then
            (push queue_st new_st,
             Map.insertWith choose new_st (new_st : steps_so_far) visited_st
            )
          else
            (queue_st, visited_st)
          )
        (queue old, visited old)
        new_states
      mk_search_state (new_current, remaining_queue) = SearchState {
        current = new_current,
        queue = remaining_queue,
        visited = new_visited
        }
  in mk_search_state <$> pop new_queue


-- | Workhorse simple search algorithm, generalized over search container
search :: (Ord state, SearchContainer f) =>
  f state
  -- ^ Empty 'SearchContainer'
  -> ([state] -> [state] -> [state])
  -- ^ Function `choose`, which when given a choice between an `old` and a `new`
  -- path to a state, `choose old new` returns either `old` or `new` as
  -- appropriate
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
search empty choose next solved prunes initial =
  let get_steps search_st = visited search_st Map.! current search_st
  in fmap (reverse . get_steps)
     . findIterate (solved . current) (nextSearchState choose next prunes)
     $ SearchState initial empty (Map.singleton initial [])

class SearchContainer f where
  pop :: Ord a => f a -> Maybe (a, f a)
  push :: Ord a => f a -> a -> f a

instance SearchContainer Seq.Seq where
  pop s =
    case Seq.viewl s of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push s a = s Seq.|> a

instance SearchContainer [] where
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

instance SearchContainer Set.Set where
  pop = Set.minView
  push = flip Set.insert

findIterate ::
  (a -> Bool)
  -> (a -> Maybe a)
  -> a
  -> Maybe a
findIterate found next initial
  | found initial = Just initial
  | otherwise = next initial >>= findIterate found next
