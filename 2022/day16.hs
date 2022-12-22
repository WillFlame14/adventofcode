import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Utils

-- Part 1 wasn't *that* bad, after some thinking. Didn't even need memoization, which was nice.
-- Then I rewrote my solution for part 2 several times after I kept trying to brute force way too many combinations.
-- This day ended up being awfully hard.

-- Part 1 Observation: There are a lot of valves with flow rate 0. We can ignore them and focus on a path as a list of non-zero valve rooms.
-- This is able to reduce the number of rooms down to 15, and then brute force dfs is able to find the path that releases the most pressure.

-- Part 2 Observation: Your path and resulting airflow can be treated independently of the elephant's path and airflow.
-- Thus, we can partition the set of non-zero rooms such that part 1 is ran twice, once on a set and once on the set's complement.
-- Find all partitions of every size, making sure to exclude duplicate work and assuming the minimum number of non-zero rooms visited is 3.
-- This is finally able to complete in a reasonable timeframe (~7.5 minutes). GG!
type Valve = (Int, [String])

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        valves = map parse contents
        connect_matrix = Map.fromList $ valves
        nonzero_valves = map fst $ filter (\(_, (flow, _)) -> flow > 0) valves
        getDistances = \valve ->
            allDistances connect_matrix (Set.singleton valve) ([(valve, 0)])
        all_combs = map (flip combinations nonzero_valves) [3..((length nonzero_valves) `div` 2)]
        distance_matrix = Map.fromList $ map (\x -> (x, Map.fromList $ getDistances x)) ("AA":nonzero_valves)
        part1 = dfs connect_matrix distance_matrix Set.empty ["AA"] (0, 0) 0 0
        in show (maximum $ map (\x -> doublePath connect_matrix distance_matrix x (filter (not . flip elem x) nonzero_valves)) (concat all_combs))

parse :: String -> (String, Valve)
parse xs =
    let parts = words xs
        name = parts !! 1
        flow_rate = read . drop 5 $ init (parts !! 4)
        connections = map (\x -> if length x == 3 then init x else x) $ drop 9 parts
        in (name, (flow_rate, connections))

-- Finds all combinations of size k from the given array.
-- Credit to stackoverflow for this function.
combinations:: Int -> [String] -> [[String]]
combinations k ns = filter ((k ==) . length) $ subsequences ns

-- Given two disjoint sets, calculates the maximum flow rate of two people running part 1 on them.
doublePath :: Map.Map String Valve -> Map.Map String (Map.Map String Int) -> [String] -> [String] -> Int
doublePath connect_matrix distance_matrix set1 set2 =
    let [right, left] = map (\s -> dfs connect_matrix distance_matrix (Set.fromList s) ["AA"] (0, 0) 0 0) [set1, set2]
        in left + right

-- Given an initial node and a starting distance of 0, returns an array of (<node>, <shortest distance to node>) via BFS.
allDistances :: Map.Map String Valve -> Set.Set String -> [(String, Int)] -> [(String, Int)]
allDistances _ _ [] = []
allDistances connect_matrix visited ((curr, dist):to_visit) =
    let (_, connections) = fromJust $ Map.lookup curr connect_matrix
        unvisited_conn = filter (\x -> Set.notMember x visited) connections
        new_visited = foldr Set.insert visited unvisited_conn
        new_to_visit = reverse . foldr (:) (reverse to_visit) . map (\x -> (x, dist + 1)) $ unvisited_conn
        in (curr, dist):(allDistances connect_matrix new_visited new_to_visit)

-- Brute forces visiting all locations in the connect_matrix and returns the highest flow rate possible.
dfs :: Map.Map String Valve -> Map.Map String (Map.Map String Int) -> Set.Set String -> [String] -> (Int, Int) -> Int -> Int -> Int
dfs connect_matrix distance_matrix visited path@(curr:p) f@(curr_flow, total_flow) time travel_time
    | time == 26        = curr_flow + total_flow
    | travel_time > 0   = dfs connect_matrix distance_matrix visited path (curr_flow, curr_flow + total_flow) (time + 1) (travel_time - 1)
    | otherwise         =
        let new_visited = Set.insert curr visited
            unvisited = filter (\x -> Set.notMember x new_visited) $ Map.keys distance_matrix
            new_curr_flow = curr_flow + (fst . fromJust $ Map.lookup curr connect_matrix)
            new_total_flow = total_flow + curr_flow
            in if null unvisited then
                dfs connect_matrix distance_matrix new_visited ("AA":path) (new_curr_flow, new_total_flow) (time + 1) 0
            else
                foldr (\y acc ->
                    let new_travel_time = fromJust . Map.lookup y . fromJust $ Map.lookup curr distance_matrix
                        in max acc $ dfs connect_matrix distance_matrix new_visited (y:path) (new_curr_flow, new_total_flow) (time + 1) new_travel_time
                    ) 0 unvisited