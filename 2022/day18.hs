import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Control.Exception
import Utils

main = interact sol

-- I stayed up until 3:30am to solve this. It was almost worth it.
-- I overcomplicated part 1 trying to find connecting points when brute force works.
-- For part 2, I find all pockets next to existing blocks and try to dfs the entire pocket, stopping if the search goes out of bounds.
-- Then I calculate its surface area and subtract it from part 1.
sol :: String -> String
sol input =
    let contents = lines input
        point_list = map parse contents
        point_set = Set.fromList point_list
        start = parse $ head contents
        pocket_heads = nub . concat $ map (possiblePockets point_set) point_list
        pockets = Set.toList . Set.union . map snd .  filter (\(b, _) -> b) $ map (findPocket point_set Set.empty) pocket_heads
        part1_ans = surfaceArea point_list
        in show (part1_ans, part1_ans - surfaceArea pockets)

-- Checks the 6 adjacent points of every block and sums the total number of faces.
surfaceArea :: [Point3] -> Int
surfaceArea point_list = sum $ map ((-) 6 . length . filter (flip Set.member $ Set.fromList point_list) . findAdjacent) point_list

-- DFS from the given point to find a pocket of air.
-- If going out of bounds, short-circuit the DFS and terminate.
findPocket :: Set.Set Point3 -> Set.Set Point3 -> Point3 -> (Bool, Set.Set Point3)
findPocket point_set visited p@(x, y, z) =
    -- trace (show p) $
    let adjacent = filter (not . flip Set.member point_set) $ findAdjacent p
        new_visited = Set.insert p visited
        in foldr (\to_visit@(x', y', z') (ok, acc_vis) ->
            if not ok then
                (False, Set.empty)
            else if (maximum [x', y', z'] > 22 || minimum [x', y', z'] < -1) then
                (False, Set.empty)
            else if Set.member to_visit acc_vis then
                (ok, acc_vis)
            else
                findPocket point_set acc_vis to_visit
            ) (True, new_visited) adjacent

-- Finds all possible adjacent pockets, given a set of all blocks and one block.
possiblePockets :: Set.Set Point3 -> Point3 -> [Point3]
possiblePockets point_set p@(x, y, z) =
    let spaces = filter (not . flip Set.member point_set) $ findAdjacent p
        in filter spaces -- (\p' -> possiblePocket point_set p (p' `point3Add` (p `point3Mult` (-1)))) spaces

possiblePocket :: Set.Set Point3 -> Point3 -> Point3 -> Bool
possiblePocket point_set point dir = any (flip Set.member point_set) . take 22 . drop 1 $ iterate (point3Add dir) point

parse :: String -> Point3
parse xs = let [x, y, z] = map read $ wordsWhen (/= ',') xs in (x, y, z)

-- Returns a list of adjacent 3D points.
findAdjacent :: Point3 -> [Point3]
findAdjacent (x, y, z) = concat $ concat [[[(x + dx, y + dy, z + dz) | dz <- [-1..1], (abs dz + abs dy + abs dx) == 1] | dy <- [-1..1]] | dx <- [-1..1]]
