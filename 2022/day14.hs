import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Utils

type Point = (Int, Int)

width = 1000
height = 200

-- Today was pretty hard.
-- I started with using a grid but Haskell instantly ate up all my memory, so I switched to using a set of obstructed points.
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        grid = [['.' | x <- [1..width]] | y <- [1..height]]
        wall_points = concat $ map (getPairs . map toPoint . concat . wordsWhen (/= "->") . words) contents
        obstruct = foldr (\x acc -> Set.union acc . Set.fromList $ drawWall x) Set.empty wall_points
        highest_y = (maximum $ map (\(p1, p2) -> max (snd p1) (snd p2)) wall_points) + 2
        part1 = (fromJust $ findIndex (\(b, _, _) -> b) (iterate (dropSand (500, 0) highest_y) (False, False, obstruct))) - 1
        part2 = (fromJust $ findIndex (\(_, b, _) -> b) (iterate (dropSand (500, 0) highest_y) (False, False, obstruct)))
        in show (part1, part2)

drawWall :: (Point, Point) -> [Point]
drawWall ((x1, y1), (x2, y2)) =
    if y1 == y2 then
        [(x, y1) | x <- [min x1 x2..max x1 x2]]
    else
        [(x1, y) | y <- [min y1 y2..max y1 y2]]

-- Drops a sand ball from the given point, given the "highest y" value (2 above the highest wall y-coordinate).
-- Returns (flag if ball fell to highest y, flag if ball obstructed (500, 0), new set of obstructed points).
dropSand :: Point -> Int -> (Bool, Bool, Set.Set Point) -> (Bool, Bool, Set.Set Point)
dropSand (x, y) highest_y (_, _, obstruct) =
    if y == highest_y - 1 then
        (True, False, Set.insert (x, y) obstruct)
    else
        let dropped = find (\(x', y') -> Set.notMember (x + x', y + y') obstruct) [(0, 1), (-1, 1), (1, 1)]
            in case dropped of
                Just (x', y')   -> dropSand (x + x', y + y') highest_y (False, False, obstruct)
                Nothing         -> if (x, y) == (500, 0) then (False, True, obstruct) else (False, False, Set.insert (x, y) obstruct)

toPoint :: String -> Point
toPoint s =
    let [a, b] = wordsWhen (/= ',') s
        in (read a, read b)

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (x1:[]) = []
getPairs (x1:x2:xs) = (x1, x2):getPairs (x2:xs)
