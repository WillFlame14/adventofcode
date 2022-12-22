import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

-- A cursed solution for part 2. It should work, but the number of combinations it needs to go through is truly staggering (prob larger than 10^97).
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
        distance_matrix = Map.fromList $ map (\x -> (x, Map.fromList $ getDistances x)) ("AA":nonzero_valves)
        in show (dfs2 connect_matrix distance_matrix Set.empty (["AA"], ["AA"]) (0, 0) 0 (0, 0))

parse :: String -> (String, Valve)
parse xs =
    let parts = words xs
        name = parts !! 1
        flow_rate = read . drop 5 $ init (parts !! 4)
        connections = map (\x -> if length x == 3 then init x else x) $ drop 9 parts
        in (name, (flow_rate, connections))

allDistances :: Map.Map String Valve -> Set.Set String -> [(String, Int)] -> [(String, Int)]
allDistances _ _ [] = []
allDistances connect_matrix visited ((curr, dist):to_visit) =
    let (_, connections) = fromJust $ Map.lookup curr connect_matrix
        unvisited_conn = filter (\x -> Set.notMember x visited) connections
        new_visited = foldr Set.insert visited unvisited_conn
        new_to_visit = reverse . foldr (:) (reverse to_visit) . map (\x -> (x, dist + 1)) $ unvisited_conn
        in (curr, dist):(allDistances connect_matrix new_visited new_to_visit)

dfs :: Map.Map String Valve -> Map.Map String (Map.Map String Int) -> Set.Set String -> [String] -> (Int, Int) -> Int -> Int -> Int
dfs connect_matrix distance_matrix visited path@(curr:p) f@(curr_flow, total_flow) time travel_time
    | time == 30        = --trace (show (reverse $ if travel_time == 0 then path else p) ++ " flow: " ++ show f) $ 
                            curr_flow + total_flow
    | travel_time > 0   = dfs connect_matrix distance_matrix visited path (curr_flow, curr_flow + total_flow) (time + 1) (travel_time - 1)
    | otherwise         =
        let new_visited = Set.insert curr visited
            unvisited = filter (\x -> Set.notMember x new_visited) $ Map.keys distance_matrix
            new_curr_flow = curr_flow + (fst . fromJust $ Map.lookup curr connect_matrix)
            new_total_flow = total_flow + curr_flow
            in if null unvisited then
                dfs connect_matrix distance_matrix new_visited ("AA":path) (new_curr_flow, new_total_flow) (time + 1) 0
            else
                foldr (\x acc ->
                    let new_travel_time = fromJust . Map.lookup x . fromJust $ Map.lookup curr distance_matrix
                        in --trace (x ++ ", path: " ++ (show (curr:path)) ++ " time: " ++ show time ++ " new_time: " ++ show new_time) .
                            max acc $ dfs connect_matrix distance_matrix new_visited (x:path) (new_curr_flow, new_total_flow) (time + 1) new_travel_time
                    ) 0 unvisited

dfs2 :: Map.Map String Valve -> Map.Map String (Map.Map String Int) -> Set.Set String -> ([String], [String]) -> (Int, Int) -> Int -> (Int, Int) -> Int
dfs2 connect_matrix distance_matrix visited (path1@(curr1:p'1), path2@(curr2:p'2)) f@(curr_flow, total_flow) time (travel1, travel2)
    | time == 26    = --trace (show (reverse path1, reverse path2) ++ " flow: " ++ show (curr_flow + total_flow)) $
                        curr_flow + total_flow
    | curr1 == "AA" && curr2 == "AA" && time > 0 =
        dfs2 connect_matrix distance_matrix visited (path1,path2) (curr_flow, curr_flow + total_flow) (time + 1) (0, 0)
    | travel1 > 0 && travel2 > 0 = 
        dfs2 connect_matrix distance_matrix visited (path1, path2) (curr_flow, curr_flow + total_flow) (time + 1) (travel1 - 1, travel2 - 1)
    | otherwise     =
        let [p1, p2] = map (== 0) [travel1, travel2]
            just_visited = if p1 && p2 then [curr1, curr2] else if p1 then [curr1] else [curr2]
            new_visited = foldr Set.insert visited just_visited
            unvisited = filter (\x -> Set.notMember x new_visited && not (x `elem` [curr1, curr2])) $ Map.keys distance_matrix
            new_curr_flow = curr_flow + (sum $ map (\x -> fst . fromJust $ Map.lookup x connect_matrix) just_visited)
            new_total_flow = total_flow + curr_flow
            in if null unvisited then
                let remaining = filter (\x -> x /= "AA" && not (x `elem` just_visited)) [curr1, curr2]
                    in if null remaining then
                        dfs2 connect_matrix distance_matrix new_visited (("AA":path1),("AA":path2)) (new_curr_flow, new_total_flow) (time + 1) (0, 0)
                    else
                        let new_travel_time = fromJust . Map.lookup (head remaining) . fromJust $ Map.lookup (if p1 then curr1 else curr2) distance_matrix
                            shortcut = new_travel_time < (if p1 then travel2 else travel1)
                            in if (False && shortcut) then
                                let new_paths = if p1 then (curr2:path1, "AA":p'2) else ("AA":p'1,curr1:path2)
                                    new_travel_times = if p1 then (new_travel_time, 0) else (0, new_travel_time)
                                    in dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
                            else
                                let new_paths = if p1 then (("AA":path1), path2) else (path1, ("AA":path2))
                                    new_travel_times = if p1 then (0, travel2 - 1) else (travel1 - 1, 0)
                                    in dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
            else
                foldr (\x acc ->
                    let new_travel_time = fromJust . Map.lookup x . fromJust $ Map.lookup (if p1 then curr1 else curr2) distance_matrix
                        in if not (p1 && p2) then
                            let new_paths = if p1 then (x:path1, path2) else (path1, x:path2)
                                new_travel_times = if p1 then (new_travel_time, travel2 - 1) else (travel1 - 1, new_travel_time)
                                in --trace (if p1 then "p1 " else "p2 " ++ x ++ ", path: " ++ (show $ let (xx, yy) = new_paths in (reverse xx, reverse yy))) .
                                    max acc $ dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
                        else
                            --trace (x ++ " " ++ (show $ filter (/= x) unvisited))
                            max acc . foldr (\y acc' ->
                                let new_paths = (x:path1, y:path2)
                                    new_travel_time' = fromJust . Map.lookup y . fromJust $ Map.lookup curr2 distance_matrix
                                    new_travel_times = (new_travel_time, new_travel_time')
                                    in --trace (x ++ " " ++ y ++ ", path: " ++ (show $ let (xx, yy) = new_paths in (reverse xx, reverse yy))) .
                                        max acc' $ dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
                                ) 0 $ filter (/= x) unvisited
                    ) 0 unvisited
                -- maximum $ map (\v ->
                --     let origin = (if v == 2 then curr2 else if p1 then curr1 else curr2)
                --         in foldr (\x acc ->
                --             let new_travel_time = fromJust . Map.lookup x . fromJust $ Map.lookup origin distance_matrix
                --                 in if not (p1 && p2) then
                --                     let new_paths = if p1 then (x:path1, path2) else (path1, x:path2)
                --                         new_travel_times = if p1 then (new_travel_time, travel2 - 1) else (travel1 - 1, new_travel_time)
                --                         in --trace (if p1 then "p1 " else "p2 " ++ x ++ ", path: " ++ (show $ let (xx, yy) = new_paths in (reverse xx, reverse yy))) .
                --                             max acc $ dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
                --                 else
                --                     --trace (x ++ " " ++ (show $ filter (/= x) unvisited))
                --                     max acc . foldr (\y acc' ->
                --                         let new_paths = if v == 1 then (x:path1, y:path2) else (y:path1, x:path2)
                --                             new_travel_time' = fromJust . Map.lookup y . fromJust $ Map.lookup (if v == 1 then curr2 else curr1) distance_matrix
                --                             new_travel_times = if v == 1 then (new_travel_time, new_travel_time') else (new_travel_time', new_travel_time)
                --                             in --trace (x ++ " " ++ y ++ ", path: " ++ (show $ let (xx, yy) = new_paths in (reverse xx, reverse yy))) .
                --                                 max acc' $ dfs2 connect_matrix distance_matrix new_visited new_paths (new_curr_flow, new_total_flow) (time + 1) new_travel_times
                --                         ) 0 $ filter (/= x) unvisited
                --             ) 0 unvisited
                -- ) [1..1]