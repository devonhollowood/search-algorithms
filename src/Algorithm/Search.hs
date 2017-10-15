{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains a collection of generalized graph search algorithms,
-- for when you don't want to explicitly represent your data as a graph. The
-- general idea is to provide these algorithms with a way of generating "next"
-- states, a way of generating associated information, a way of determining
-- when you have found a solution, and an initial state.
module Algorithm.Search (
  -- * Searches
  bfs,
  dfs,
  dijkstra,
  aStar,
  -- * Monadic Searches
  -- $monadic
  bfsM,
  dfsM,
  dijkstraM,
  aStarM,
  -- * Utility
  incrementalCosts,
  incrementalCostsM,
  pruning,
  pruningM
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Control.Monad (filterM, zipWithM)

-- | @bfs next found initial@ performs a breadth-first search over a set of
-- states, starting with @initial@, and generating neighboring states with
-- @next@. It returns a path to a state for which @found@ returns 'True'.
-- Returns 'Nothing' if no path is possible.
--
-- === Example: Making change problem
--
-- >>> :{
-- countChange target = bfs (add_one_coin `pruning` (> target)) (== target) 0
--   where
--     add_one_coin amt = map (+ amt) coins
--     coins = [25, 10, 5, 1]
-- :}
--
-- >>> countChange 67
-- Just [25,50,60,65,66,67]
bfs :: (Foldable f, Ord state)
  => (state -> f state)
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'bfs' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
bfs =
  -- BFS is a generalized search using a queue, which directly compares states,
  -- and which always uses the first path found to a state
  generalizedSearch Seq.empty id (\_ _ -> False)


-- | @dfs next found initial@ performs a depth-first search over a set
-- of states, starting with @initial@ and generating neighboring states with
-- @next@. It returns a depth-first path to a state for which @found@ returns
-- 'True'. Returns 'Nothing' if no path is possible.
--
-- === Example: Simple directed graph search
--
-- >>> import qualified Data.Map as Map
--
-- >>> graph = Map.fromList [(1, [2, 3]), (2, [4]), (3, [4]), (4, [])]
--
-- >>> dfs (graph Map.!) (== 4) 1
-- Just [3,4]
dfs :: (Foldable f, Ord state)
  => (state -> f state)
  -- ^ Function to generate "next" states given a current state. These should be
  -- given in the order in which states should be pushed onto the stack, i.e.
  -- the "last" state in the Foldable will be the first one visited.
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dfs' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
dfs =
  -- DFS is a generalized search using a stack, which directly compares states,
  -- and which always uses the most recent path found to a state
  generalizedSearch [] id (\_ _ -> True)


-- | @dijkstra next cost found initial@ performs a shortest-path search over
-- a set of states using Dijkstra's algorithm, starting with @initial@,
-- generating neighboring states with @next@, and their incremental costs with
-- @costs@. This will find the least-costly path from an initial state to a
-- state for which @found@ returns 'True'. Returns 'Nothing' if no path to a
-- solved state is possible.
--
-- === Example: Making change problem, with a twist
--
-- >>> :{
-- -- Twist: dimes have a face value of 10 cents, but are actually rare
-- -- misprints which are worth 10 dollars
-- countChange target =
--   dijkstra (add_coin `pruning` (> target)) true_cost  (== target) 0
--   where
--     coin_values = [(25, 25), (10, 1000), (5, 5), (1, 1)]
--     add_coin amt = map ((+ amt) . snd) coin_values
--     true_cost low high =
--       case lookup (high - low) coin_values of
--         Just val -> val
--         Nothing -> error $ "invalid costs: " ++ show high ++ ", " ++ show low
-- :}
--
-- >>> countChange 67
-- Just (67,[1,2,7,12,17,42,67])
dijkstra :: (Foldable f, Num cost, Ord cost, Ord state)
  => (state -> f state)
  -- ^ Function to generate list of neighboring states given the current state
  -> (state -> state -> cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dijkstra' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe (cost, [state])
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstra next cost found initial =
  -- Dijkstra's algorithm can be viewed as a generalized search, with the search
  -- container being a heap, with the states being compared without regard to
  -- cost, with the shorter paths taking precedence over longer ones, and with
  -- the stored state being (cost so far, state).
  -- This implementation makes that transformation, then transforms that result
  -- back into the desired result from @dijkstra@
  unpack <$>
    generalizedSearch emptyLIFOHeap snd leastCostly next' (found . snd)
      (0, initial)
  where
    next' (old_cost, st) =
      (\new_st -> (cost st new_st + old_cost, new_st))
        <$> Foldable.toList (next st)
    unpack [] = (0, [])
    unpack packed_states = (fst . last $ packed_states, map snd packed_states)


-- | @aStar next cost remaining found initial@ performs a best-first search
-- using the A* search algorithm, starting with the state @initial@, generating
-- neighboring states with @next@, their cost with @cost@, and an estimate of
-- the remaining cost with @remaining@. This returns a path to a state for which
-- @found@ returns 'True'. If @remaining@ is strictly a lower bound on the
-- remaining cost to reach a solved state, then the returned path is the
-- shortest path. Returns 'Nothing' if no path to a solved state is possible.
--
-- === Example: Path finding in taxicab geometry
--
-- >>> :{
-- neighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
-- dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)
-- start = (0, 0)
-- end = (0, 2)
-- isWall = (== (0, 1))
-- :}
--
-- >>> aStar (neighbors `pruning` isWall) dist (dist end) (== end) start
-- Just (4,[(1,0),(1,1),(1,2),(0,2)])
aStar :: (Foldable f, Num cost, Ord cost, Ord state)
  => (state -> f state)
  -- ^ Function which, when given the current state, produces a list whose
  -- elements are (incremental cost to reach neighboring state,
  -- estimate on remaining cost from said state, said state).
  -> (state -> state -> cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> cost)
  -- ^ Estimate on remaining cost given a state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'aStar' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe (cost, [state])
  -- ^ (Total cost, list of steps) for the first path found which satisfies the
  -- given predicate
aStar next cost remaining found initial =
  -- A* can be viewed as a generalized search, with the search container being a
  -- heap, with the states being compared without regard to cost, with the
  -- shorter paths taking precedence over longer ones, and with
  -- the stored state being (total cost estimate, (cost so far, state)).
  -- This implementation makes that transformation, then transforms that result
  -- back into the desired result from @aStar@
  unpack <$> generalizedSearch emptyLIFOHeap snd2 leastCostly next'
    (found . snd2) (remaining initial, (0, initial))
  where
    next' (_, (old_cost, old_st)) =
      update_state <$> Foldable.toList (next old_st)
      where
        update_state new_st =
          let new_cost = old_cost + cost old_st new_st
              new_est = new_cost + remaining new_st
          in (new_est, (new_cost, new_st))
    unpack [] = (0, [])
    unpack packed_states =
      (fst . snd . last $ packed_states, map snd2 packed_states)
    snd2 = snd . snd

-- $monadic
-- Note that for all monadic searches, it is up to the user to ensure that
-- side-effecting monads do not logically change the structure of the graph.
-- For example, if the list of neighbors is being read from a file, the user
-- must ensure that those values do not change between reads.

-- | @bfsM@ is a monadic version of 'bfs': it has support for monadic @next@ and
-- @found@ parameters.
bfsM :: (Monad m, Foldable f, Ord state)
  => (state -> m (f state))
  -- ^ Function to generate "next" states given a current state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'bfsM' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe [state])
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
bfsM = generalizedSearchM Seq.empty id (\_ _ -> False)


-- | @dfsM@ is a monadic version of 'dfs': it has support for monadic @next@ and
-- @found@ parameters.
dfsM :: (Monad m, Foldable f, Ord state)
  => (state -> m (f state))
  -- ^ Function to generate "next" states given a current state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'dfsM' returns a path to the
  -- first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe [state])
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
dfsM =
  generalizedSearchM [] id (\_ _ -> True)

-- | @dijkstraM@ is a monadic version of 'dijkstra': it has support for monadic
-- @next@, @cost@, and @found@ parameters.
dijkstraM :: (Monad m, Foldable f, Num cost, Ord cost, Ord state)
  => (state -> m (f state))
  -- ^ Function to generate list of neighboring states given the current state
  -> (state -> state -> m cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'dijkstraM' returns the
  -- shortest path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstraM nextM costM foundM initial =
  fmap2 unpack $ generalizedSearchM emptyLIFOHeap snd leastCostly nextM'
    (foundM . snd) (0, initial)
  where
    nextM' (old_cost, old_st) = do
      new_states <- Foldable.toList <$> nextM old_st
      incr_costs <- sequence $ costM old_st <$> new_states
      let new_costs = (+ old_cost) <$> incr_costs
      return $ zip new_costs new_states
    unpack [] = (0, [])
    unpack packed_states = (fst . last $ packed_states, map snd packed_states)


-- | @aStarM@ is a monadic version of 'aStar': it has support for monadic
-- @next@, @cost@, @remaining@, and @found@ parameters.
aStarM :: (Monad m, Foldable f, Num cost, Ord cost, Ord state)
  => (state -> m (f state))
  -- ^ Function which, when given the current state, produces a list whose
  -- elements are (incremental cost to reach neighboring state,
  -- estimate on remaining cost from said state, said state).
  -> (state -> state -> m cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> m cost)
  -- ^ Estimate on remaining cost given a state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'aStarM' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which satisfies the
  -- given predicate
aStarM nextM costM remainingM foundM initial = do
  remaining_init <- remainingM initial
  fmap2 unpack $ generalizedSearchM emptyLIFOHeap snd2 leastCostly nextM'
    (foundM . snd2) (remaining_init, (0, initial))
  where
    nextM' (_, (old_cost, old_st)) = do
      new_states <- Foldable.toList <$> nextM old_st
      sequence $ update_stateM <$> new_states
      where
        update_stateM new_st = do
          remaining <- remainingM new_st
          cost <- costM old_st new_st
          let new_cost = old_cost + cost
              new_est = new_cost + remaining
          return (new_est, (new_cost, new_st))
    unpack [] = (0, [])
    unpack packed_states =
      (fst . snd . last $ packed_states, map snd2 packed_states)
    snd2 = snd . snd


-- | @incrementalCosts cost_fn states@ gives a list of the incremental costs
-- going from state to state along the path given in @states@, using the cost
-- function given by @cost_fn@. Note that the paths returned by the searches
-- in this module do not include the initial state, so if you want the
-- incremental costs along a @path@ returned by one of these searches, you
-- want to use @incrementalCosts cost_fn (initial : path)@.
--
-- === Example: Getting incremental costs from dijkstra
--
-- >>> import Data.Maybe (fromJust)
--
-- >>> :{
-- cyclicWeightedGraph :: Map.Map Char [(Char, Int)]
-- cyclicWeightedGraph = Map.fromList [
--   ('a', [('b', 1), ('c', 2)]),
--   ('b', [('a', 1), ('c', 2), ('d', 5)]),
--   ('c', [('a', 1), ('d', 2)]),
--   ('d', [])
--   ]
-- start = (0, 0)
-- end = (0, 2)
-- cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
-- :}
--
-- >>> incrementalCosts cost ['a', 'b', 'd']
-- [1,5]
incrementalCosts ::
  (state -> state -> cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states in the `states` list, so it is safe to have
  -- this function be partial for non-neighboring states.
  -> [state]
  -- ^ A path, given as a list of adjacent states, along which to find the
  -- incremental costs
  -> [cost]
  -- ^ List of incremental costs along given path
incrementalCosts cost_fn states = zipWith cost_fn states (tail states)

-- | @incrementalCostsM@ is a monadic version of 'incrementalCosts': it has
-- support for a monadic @const_fn@ parameter.
incrementalCostsM ::
  (Monad m) =>
  (state -> state -> m cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states in the `states` list, so it is safe to have
  -- this function be partial for non-neighboring states.
  -> [state]
  -- ^ A path, given as a list of adjacent states, along which to find the
  -- incremental costs
  -> m [cost]
  -- ^ List of incremental costs along given path
incrementalCostsM costM states = zipWithM costM states (tail states)


-- | @next \`pruning\` predicate@ streams the elements generate by @next@ into a
-- list, removing elements which satisfy @predicate@. This is useful for the
-- common case when you want to logically separate your search's `next` function
-- from some way of determining when you've reached a dead end.
--
-- === Example: Pruning a Set
--
-- >>> import qualified Data.Set as Set
--
-- >>> ((\x -> Set.fromList [0..x]) `pruning` even) 10
-- [1,3,5,7,9]
--
-- === Example: depth-first search, avoiding certain nodes
--
-- >>> import qualified Data.Map as Map
--
-- >>> :{
-- graph = Map.fromList [
--   ('a', ['b', 'c', 'd']),
--   ('b', [undefined]),
--   ('c', ['e']),
--   ('d', [undefined]),
--   ('e', [])
--   ]
-- :}
--
-- >>> dfs ((graph Map.!) `pruning` (`elem` "bd")) (== 'e') 'a'
-- Just "ce"
pruning ::
  (Foldable f)
  => (a -> f a)
  -- ^ Function to generate next states
  -> (a -> Bool)
  -- ^ Predicate to prune on
  -> (a -> [a])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
next `pruning` predicate =
  (filter (not . predicate) . Foldable.toList) <$> next


-- | @pruningM@ is a monadic version of 'pruning': it has support for monadic
-- @next@ and @predicate@ parameters
pruningM ::
  (Monad m, Foldable f)
  => (a -> m (f a))
  -- ^ Function to generate next states
  -> (a -> m Bool)
  -- ^ Predicate to prune on
  -> (a -> m [a])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
pruningM nextM predicateM a = do
  next_states <- nextM a
  filterM (fmap not. predicateM) $ Foldable.toList next_states


-- | A @SearchState@ represents the state of a generalized search at a given
-- point in an algorithms execution. The advantage of this abstraction is that
-- it can be used for things like bidirectional searches, where you want to
-- stop and start a search part-way through.
data SearchState container stateKey state = SearchState {
  current :: state,
  queue :: container,
  visited :: Set.Set stateKey,
  paths :: Map.Map stateKey [state]
  }


-- | @nextSearchState@ moves from one @searchState@ to the next in the
-- generalized search algorithm
nextSearchState ::
  (Foldable f, SearchContainer container, Ord stateKey, Elem container ~ state)
  => ([state] -> [state] -> Bool)
  -> (state -> stateKey)
  -> (state -> f state)
  -> SearchState container stateKey state
  -> Maybe (SearchState container stateKey state)
nextSearchState better mk_key next old = do
  new_state <- mk_search_state <$> pop new_queue
  if mk_key (current new_state) `Set.member` visited old
    then nextSearchState better mk_key next new_state
    else Just new_state
  where
    mk_search_state (new_current, remaining_queue) = SearchState {
      current = new_current,
      queue = remaining_queue,
      visited = Set.insert (mk_key new_current) (visited old),
      paths = new_paths
      }
    new_states = next (current old)
    (new_queue, new_paths) =
      List.foldl' update_queue_paths (queue old, paths old) new_states
    update_queue_paths (old_queue, old_paths) st =
      if mk_key st `Set.member` visited old
      then (old_queue, old_paths)
      else
        case Map.lookup (mk_key st) old_paths of
          Just old_path ->
            if better old_path (st : steps_so_far)
            then (q', ps')
            else (old_queue, old_paths)
          Nothing -> (q', ps')
        where
          steps_so_far = paths old Map.! mk_key (current old)
          q' = push old_queue st
          ps' = Map.insert (mk_key st) (st : steps_so_far) old_paths


-- | Workhorse simple search algorithm, generalized over search container
-- and path-choosing function. The idea here is that many search algorithms are
-- at their core the same, with these details substituted. By writing these
-- searches in terms of this function, we reduce the chances of errors sneaking
-- into each separate implementation.
generalizedSearch ::
  (Foldable f, SearchContainer container, Ord stateKey, Elem container ~ state)
  => container
  -- ^ Empty @SearchContainer@
  -> (state -> stateKey)
  -- ^ Function to turn a @state@ into a key by which states will be compared
  -- when determining whether a state has be enqueued and / or visited
  -> ([state] -> [state] -> Bool)
  -- ^ Function @better old new@, which when given a choice between an @old@ and
  -- a @new@ path to a state, returns True when @new@ is a "better" path than
  -- old and should thus be inserted
  -> (state -> f state)
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. @generalizedSearch@ returns a
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe [state]
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
generalizedSearch empty mk_key better next found initial =
  let get_steps search_st = paths search_st Map.! mk_key (current search_st)
  in fmap (reverse . get_steps)
     . findIterate (nextSearchState better mk_key next) (found . current)
     $ SearchState initial empty (Set.singleton $ mk_key initial)
       (Map.singleton (mk_key initial) [])


-- | @nextSearchState@ moves from one @searchState@ to the next in the
-- generalized search algorithm
nextSearchStateM ::
  (Monad m, Foldable f, SearchContainer container, Ord stateKey,
   Elem container ~ state)
  => ([state] -> [state] -> Bool)
  -> (state -> stateKey)
  -> (state -> m (f state))
  -> SearchState container stateKey state
  -> m (Maybe (SearchState container stateKey state))
nextSearchStateM better mk_key nextM old = do
  (new_queue, new_paths) <- new_queue_paths_M
  let new_state_May = mk_search_state new_paths <$> pop new_queue
  case new_state_May of
    Just new_state ->
      if mk_key (current new_state) `Set.member` visited old
      then nextSearchStateM better mk_key nextM new_state
      else return (Just new_state)
    Nothing -> return Nothing
  where
    mk_search_state new_paths (new_current, remaining_queue) = SearchState {
      current = new_current,
      queue = remaining_queue,
      visited = Set.insert (mk_key new_current) (visited old),
      paths = new_paths
      }
    new_queue_paths_M =
      List.foldl' update_queue_paths (queue old, paths old)
        <$> nextM (current old)
    update_queue_paths (old_queue, old_paths) st =
      if mk_key st `Set.member` visited old
      then (old_queue, old_paths)
      else
        case Map.lookup (mk_key st) old_paths of
          Just old_path ->
            if better old_path (st : steps_so_far)
            then (q', ps')
            else (old_queue, old_paths)
          Nothing -> (q', ps')
        where
          steps_so_far = paths old Map.! mk_key (current old)
          q' = push old_queue st
          ps' = Map.insert (mk_key st) (st : steps_so_far) old_paths


-- | @generalizedSearchM@ is a monadic version of generalizedSearch
generalizedSearchM ::
  (Monad m, Foldable f, SearchContainer container, Ord stateKey,
   Elem container ~ state)
  => container
  -- ^ Empty @SearchContainer@
  -> (state -> stateKey)
  -- ^ Function to turn a @state@ into a key by which states will be compared
  -- when determining whether a state has be enqueued and / or visited
  -> ([state] -> [state] -> Bool)
  -- ^ Function @better old new@, which when given a choice between an @old@ and
  -- a @new@ path to a state, returns True when @new@ is a "better" path than
  -- old and should thus be inserted
  -> (state -> m (f state))
  -- ^ Function to generate "next" states given a current state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. @generalizedSearch@ returns a
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe [state])
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
generalizedSearchM empty mk_key better nextM foundM initial = do
  let initial_state =
        SearchState initial empty (Set.singleton $ mk_key initial)
        (Map.singleton (mk_key initial) [])
  end_May <- findIterateM (nextSearchStateM better mk_key nextM)
    (foundM . current) initial_state
  return $ fmap (reverse . get_steps) end_May
  where
    get_steps search_st = paths search_st Map.! mk_key (current search_st)


newtype LIFOHeap k a = LIFOHeap (Map.Map k [a])


emptyLIFOHeap :: LIFOHeap k a
emptyLIFOHeap = LIFOHeap Map.empty


-- | The @SearchContainer@ class abstracts the idea of a container to be used in
-- @generalizedSearch@
class SearchContainer container where
  type Elem container
  pop :: container -> Maybe (Elem container, container)
  push :: container -> Elem container -> container

instance SearchContainer (Seq.Seq a) where
  type Elem (Seq.Seq a) = a
  pop s =
    case Seq.viewl s of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push s a = s Seq.|> a

instance SearchContainer [a] where
  type Elem [a] = a
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

instance Ord k => SearchContainer (LIFOHeap k a) where
  type Elem (LIFOHeap k a) = (k, a)
  pop (LIFOHeap inner)
    | Map.null inner = Nothing
    | otherwise = case Map.findMin inner of
      (k, [a]) -> Just ((k, a), LIFOHeap $ Map.deleteMin inner)
      (k, a : _) -> Just ((k, a), LIFOHeap $ Map.updateMin (Just . tail) inner)
      (_, []) -> pop (LIFOHeap $ Map.deleteMin inner)
                 -- Logically, this should never happen
  push (LIFOHeap inner) (k, a) = LIFOHeap $ Map.insertWith (++) k [a] inner


-- | @findIterate found next initial@ takes an initial seed value and applies
-- @next@ to it until either @found@ returns True or @next@ returns @Nothing@
findIterate :: (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
findIterate next found initial
  | found initial = Just initial
  | otherwise = next initial >>= findIterate next found


-- | @findIterateM@ is a monadic version of @findIterate@
findIterateM :: Monad m => (a -> m (Maybe a)) -> (a -> m Bool) -> a -> m (Maybe a)
findIterateM nextM foundM initial = do
  found <- foundM initial
  if found
  then return $ Just initial
  else nextM initial >>= maybe (return Nothing) (findIterateM nextM foundM)


-- | @leastCostly paths_a paths_b@ is a utility function to be used with
-- 'dijkstra'-like functions. It returns True when the cost of @paths_a@
-- is less than the cost of @paths_b@, where the total costs are the first
-- elements in each tuple in each path
leastCostly :: Ord a => [(a, b)] -> [(a, b)] -> Bool
leastCostly ((cost_a, _):_) ((cost_b, _):_) = cost_b < cost_a
-- logically this never happens, because if you have a
-- zero-length path a point, you already visited it
-- and thus do not consider other paths to it
leastCostly [] _ = False
-- logically this never happens, because you cannot find
-- a new zero-length path to a point
leastCostly _ [] = True


-- | This is just a convenience function which @fmap@s two deep
fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap
