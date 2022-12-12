import Data.Char
import qualified Data.Set as Set

-- Cue 1 hour of debugging while I try to remember how to write Dijkstra's in Haskell.
-- Part 2 takes 30 seconds to run because I just re-ran part 1 on every possible starting point. :)
type Point = (Int, Int)

main = interact sol

sol :: String -> String
sol input =
    let grid = lines input
        all_points = concat [[(x, y) | y <- [0..(length (grid !! 0) - 1)]] | x <- [0..(length grid - 1)]]
        start = head $ filter (\p -> (gridAt grid p) == 'S') all_points
        other_starts = filter (\p -> (gridAt grid p) == 'a') all_points
        in show (findPath grid (0, start) Set.empty [], minimum $ map (\s -> findPath grid (0, s) Set.empty []) other_starts)

findNeighbours :: [[Char]] -> Point -> [Point]
findNeighbours grid (sx, sy) =
    let neighbours = concat [[(sx + dx, sy + dy) | dy <- [-1..1], abs dx + abs dy == 1] | dx <- [-1..1]]
        valid = \(x, y) ->
            (x >= 0 && x < length grid) && (y >= 0 && y < length (grid !! 0)) &&
            let s = gridAt grid (sx, sy)
                e = gridAt grid (x, y)
                in ord (if s == 'S' then 'a' else s) + 1 >= ord (if e == 'E' then 'z' else e)
        in filter valid neighbours

-- Performs Dijkstra's algorithm, keeping an ordered list of (distance, pointToVisit) and a set of visited points.
-- Returns the shortest distance to the 'E', or 999999 if it cannot be reached.
findPath :: [[Char]] -> (Int, Point) -> Set.Set Point -> [(Int, Point)] -> Int
findPath grid (dist, curr) visited toVisit =
    if (gridAt grid curr) == 'E' then
        dist
    else
        let neighbours = filter (\p -> not $ Set.member p visited) $ findNeighbours grid curr
            new_toVisit = foldl visitAdd toVisit $ map (\p -> (dist + 1, p)) neighbours
            new_visited = Set.insert curr visited
            in if null new_toVisit then 999999 else
                findPath grid (head new_toVisit) new_visited (tail new_toVisit)

gridAt :: [[Char]] -> Point -> Char
gridAt grid (x, y) = (grid !! x) !! y

-- Inserts a (priority, point) into the toVisit priority queue.
-- Does not take into consideration that there may be a duplicate entry for the same point.
visitInsert :: [(Int, Point)] -> (Int, Point) -> [(Int, Point)]
visitInsert [] n = [n]
visitInsert xs@((v',p'):rest) n@(v,p) =
    if (v < v') then
        n:xs
    else
        (v',p'):(visitInsert rest n)

-- Adds a new (priority, point) to the toVisit priority queue.
-- First checks if an entry for the point already exists with an equal or lower distance
-- Then removes a duplicate of the point (if it exists) and inserts the new element in the correct position.
visitAdd :: [(Int, Point)] -> (Int, Point) -> [(Int, Point)]
visitAdd xs n@(v, p) =
    if any (\(v', p') -> p' == p && v' <= v) xs then
        xs
    else
        visitInsert (filter (\(_, p') -> p' /= p) xs) n
